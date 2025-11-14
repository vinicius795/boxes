from pathlib import Path

import pytest

import boxes


class SquareHoleBox(boxes.Boxes):
    def render(self) -> None:
        # draw a 10x10 inner square with sharp corners
        self.rectangularHole(0, 0, 10, 10, r=0, center_x=False, center_y=False)


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


@pytest.mark.parametrize("burn", [0.0, 0.1])
def test_corner_inner_corners_are_lines(burn):
    path = _square_path(burn)
    path.faster_edges("corner")
    assert all(cmd[0] != "C" for cmd in path.path)


@pytest.mark.parametrize("burn", [0.0, 0.1])
def test_backarc_preserves_original_arcs(burn):
    path = _square_path(burn)
    path.faster_edges("backarc")
    if burn == 0.0:
        assert any(cmd[0] == "I" for cmd in path.path)
    else:
        assert any(cmd[0] == "C" for cmd in path.path)


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


if __name__ == "__main__":
    dump_square_paths(Path("tests") / "inner_corners_dump.txt")
