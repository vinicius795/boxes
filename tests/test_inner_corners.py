from pathlib import Path
import copy

import pytest

import boxes
from boxes.dxf_generator import DXFSurface


class SquareHoleBox(boxes.Boxes):
    def render(self) -> None:
        # draw a 10x10 inner square with sharp corners
        self.rectangularHole(0, 0, 100, 100, r=0, center_x=False, center_y=False)


def _square_path(burn):
    box = SquareHoleBox()
    box.parseArgs(["--burn", str(burn)])
    box.metadata["reproducible"] = True
    box.open()
    box.render()
    for part in box.surface.parts:
        for path in part.pathes:
            if any(cmd[0] in ("I", "C") for cmd in path.path):
                return path
    raise AssertionError("square path not found")


@pytest.mark.parametrize("burn", [0.0, 0.1])
def test_loop_inner_corners_have_curves(burn):
    path = _square_path(burn)
    path.faster_edges("loop")
    assert sum(cmd[0] == "C" for cmd in path.path) == 4
    assert all(cmd[0] != "I" for cmd in path.path)


@pytest.mark.parametrize("burn", [0.0, 0.1])
def test_corner_inner_corners_are_lines(burn):
    path = _square_path(burn)
    path.faster_edges("corner")
    assert all(cmd[0] not in ("C", "I") for cmd in path.path)


@pytest.mark.parametrize("burn", [0.0, 0.1])
def test_backarc_preserves_original_arcs(burn):
    path = _square_path(burn)
    path.faster_edges("backarc")
    assert all(cmd[0] != "I" for cmd in path.path)
    assert sum(cmd[0] == "C" for cmd in path.path) == 4


def test_virtual_inner_corners_store_loop_geometry():
    path = _square_path(0.0)
    lw = path.params["lw"]
    seen = 0
    for idx, cmd in enumerate(path.path):
        if cmd[0] != "I":
            continue
        prev = path.path[idx - 1]
        assert prev[0] == "L"
        px, py = prev[1:3]
        ix, iy = cmd[1:3]
        dx = abs(ix - px)
        dy = abs(iy - py)
        assert max(dx, dy) == pytest.approx(lw, rel=1e-2, abs=1e-4)
        assert min(dx, dy) == pytest.approx(0.0, abs=1e-6)
        seen += 1
    assert seen == 4


def dump_square_paths(out_file: Path | None = None) -> None:
    """Create a debugging dump showing paths before/after faster_edges."""
    lines: list[str] = []
    for burn in (0.0, 0.1):
        base = _square_path(burn)
        lines.append(f"=== burn {burn} original ===")
        for cmd in base.path:
            lines.append(repr(cmd))
        for style in ("loop", "corner", "backarc"):
            path = _square_path(burn)
            path.faster_edges(style)
            lines.append(f"--- burn {burn} {style} ---")
            for cmd in path.path:
                lines.append(repr(cmd))
        lines.append("")
    text = "\n".join(lines)
    if out_file is None:
        print(text)
    else:
        out_file.write_text(text, encoding="utf-8")


def _square_svg_bytes(burn: float, inner_corners: str) -> bytes:
    box = SquareHoleBox()
    args = ["--burn", str(burn)]
    if inner_corners in {"loop", "corner", "backarc"}:
        args += ["--inner_corners", inner_corners]
    box.parseArgs(args)
    box.metadata["reproducible"] = True
    box.open()
    box.render()
    # "original" needs the raw geometry; convert virtual arcs but skip trimming.
    style = inner_corners if inner_corners != "original" else "backarc"
    box.surface.set_metadata(box.metadata)
    data = box.surface.finish(style)
    return data.getvalue()


def _square_dxf_bytes(burn: float, inner_corners: str) -> bytes:
    box = SquareHoleBox()
    box.parseArgs(["--burn", str(burn)])
    box.metadata["reproducible"] = True
    box.open()
    box.render()
    style = inner_corners if inner_corners != "original" else "backarc"
    dxf_surface = DXFSurface()
    dxf_surface.scale = box.surface.scale
    dxf_surface.invert_y = box.surface.invert_y
    dxf_surface.parts = copy.deepcopy(box.surface.parts)
    dxf_surface.count = box.surface.count
    dxf_surface.set_metadata(box.metadata)
    data = dxf_surface.finish(style)
    return data.getvalue()


def dump_square_dxfs(out_dir: Path | None = None) -> None:
    """Generate DXF files mirroring dump_square_paths for visual inspection."""
    target = Path(out_dir) if out_dir else Path("tests") / "inner_corner_dxfs"
    target.mkdir(parents=True, exist_ok=True)
    for burn in (0.0, 0.1):
        for style in ("original", "loop", "corner", "backarc"):
            dxf_bytes = _square_dxf_bytes(burn, style)
            filename = target / f"burn {burn:.1f} {style}.dxf"
            filename.write_bytes(dxf_bytes)


if __name__ == "__main__":
    dump_square_paths(Path("tests") / "inner_corners_dump.txt")
    dump_square_dxfs()
