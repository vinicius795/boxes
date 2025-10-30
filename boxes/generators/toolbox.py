
# Copyright (C) 2013-2025 Florian Festi
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program.  If not, see <http://www.gnu.org/licenses/>.

import math
from contextlib import contextmanager

from boxes import *

F_FORMAT: str = "dxf"


class FingerJointPositivePlayEdge(edges.FingerJointEdge):
    """Finger joint edge that also shrinks tabs according to play."""

    char = "x"
    description = "Finger joint (male) with play compensation"

    def __call__(self, length, bedBolts=None, bedBoltSettings=None, **kw):
        with self._positive_play_adjustment():
            return super().__call__(length, bedBolts=bedBolts, bedBoltSettings=bedBoltSettings, **kw)

    @contextmanager
    def _positive_play_adjustment(self):
        play = max(0.0, getattr(self.settings, "play", 0.0))
        original_space = self.settings.space
        original_finger = self.settings.finger
        # avoid negative finger widths
        effective = min(play, max(0.0, original_finger - 0.1 * self.settings.thickness))
        if effective <= 0.0:
            yield
            return

        self.settings.space = original_space + effective
        self.settings.finger = original_finger - effective
        try:
            yield
        finally:
            self.settings.space = original_space
            self.settings.finger = original_finger


class FingerJointDividerEdge(FingerJointPositivePlayEdge):
    """Finger joint edge that also cuts divider slots."""

    char = "w"
    description = "Finger joint with parallel divider slots"

    def __init__(self, boxes, settings=None) -> None:
        super().__init__(boxes, settings)
        self._fingerholes = getattr(boxes, "fingerHolesAt", None)

    def __call__(self, length, bedBolts=None, bedBoltSettings=None, **kw):
        with self._positive_play_adjustment():
            self._render_divider_holes(length, bedBolts, bedBoltSettings)
            super(FingerJointPositivePlayEdge, self).__call__(length, bedBolts=bedBolts,
                                                              bedBoltSettings=bedBoltSettings, **kw)

    def _render_divider_holes(self, length, bedBolts, bedBoltSettings) -> None:
        fingerholes = self._fingerholes
        if fingerholes is None or getattr(fingerholes, "settings", None) is None:
            return

        offset = getattr(self.boxes, "divider_hole_offset", 0.0)
        try:
            offset = float(offset)
        except (TypeError, ValueError):
            offset = 0.0
        dist = fingerholes.settings.edge_width
        y_offset = self.burn + dist + self.settings.thickness / 2 + offset

        with self.saved_context():
            fingerholes(0, y_offset, length, 0,
                        bedBolts=bedBolts, bedBoltSettings=bedBoltSettings)


class CustomCabinetHingeEdge(edges.BaseEdge):
    """Edge with cabinet hinges and customizable spacing segments."""

    char = "u"
    description = "Edge with cabinet hinges"

    def __init__(self, boxes, settings=None, top: bool = False, angled: bool = False) -> None:
        super().__init__(boxes, settings)
        self.top = top
        self.angled = angled
        self.char = "uUvV"[bool(top) + 2 * bool(angled)]

    def startwidth(self) -> float:
        return self.settings.thickness if self.top and self.angled else 0.0

    def _handle_profile_requirements(self) -> tuple[float, float, float] | None:
        """Return (inner_span, required_length, handle_thickness) for the handle fingers."""
        handle_width = getattr(self.boxes, "handle_width", None)
        handle_thickness = getattr(self.boxes, "handle_thickness", None)
        if handle_width is None or handle_thickness is None:
            return None

        burn = getattr(self.boxes, "burn", 0.0)
        # Handle.render (lines ~240+) introduces two shoulders, each consuming
        # burn + (0.5 + burn) millimeters before the finger tabs start.
        shoulder_allowance = 2.0 * (burn + (0.5 + burn))  # -> 1mm + 4*burn
        inner_span = max(0.0, handle_width - (2.0 * handle_thickness + shoulder_allowance))
        required = 2.0 * handle_thickness + inner_span
        return inner_span, required, handle_thickness

    def _hinge_spacing_segment(self, length: float, index: int, total: int, total_length: float) -> None:
        """Draw the straight segment that spaces hinge modules.

        ``index`` can be ``-1`` for the leading segment and ``total`` for the trailing one.
        """
        if length <= 0:
            return

        if self.char != "u":
            self.edge(length, tabs=2)
            return

        finger_edge = self.boxes.edges.get('F')
        requirements = self._handle_profile_requirements()

        if finger_edge is None or requirements is None:
            self.edge(length, tabs=2)
            return

        inner_span, required, handle_thickness = requirements
        if length < required:
            self.edge(length, tabs=2)
            return

        edges_spacing = max(0.0, (length - required) / 2.0)

        self.edge(edges_spacing)
        finger_edge(handle_thickness)
        self.edge(inner_span)
        finger_edge(handle_thickness)
        self.edge(edges_spacing)

    def _should_use_custom_spacing(self, length: float) -> bool:
        requirements = self._handle_profile_requirements()
        if requirements is None:
            return True
        _, required, _ = requirements
        return length >= required

    def __poly(self):
        n = self.settings.eyes_per_hinge
        p = self.settings.play
        e = self.settings.eye
        t = self.settings.thickness
        spacing = self.settings.spacing

        if self.settings.style == "outside" and self.angled:
            e = t
        elif self.angled and not self.top:
            # move hinge up to leave space for lid
            e -= t

        if self.top:
            # start with space
            poly = [spacing, 90, e + p]
        else:
            # start with hinge eye
            poly = [spacing + p, 90, e + p, 0]
        for i in range(n):
            if (i % 2) ^ self.top:
                # space
                if i == 0:
                    poly += [-90, t + 2 * p, 90]
                else:
                    poly += [90, t + 2 * p, 90]
            else:
                # hinge eye
                poly += [t - p, -90, t, -90, t - p]

        if (n % 2) ^ self.top:
            # stopped with hinge eye
            poly += [0, e + p, 90, p + spacing]
        else:
            # stopped with space
            poly[-1:] = [-90, e + p, 90, spacing]

        width = (t + p) * n + p + 2 * spacing

        return poly, width

    def __call__(self, l, **kw):
        n = self.settings.eyes_per_hinge
        p = self.settings.play
        e = self.settings.eye
        t = self.settings.thickness
        hn = self.settings.hinges

        poly, width = self.__poly()

        if self.settings.style == "outside" and self.angled:
            e = t
        elif self.angled and not self.top:
            # move hinge up to leave space for lid
            e -= t

        hn = min(hn, int(l // width))

        if hn == 1:
            lead = (l - width) / 2
            if self._should_use_custom_spacing(lead):
                self._hinge_spacing_segment(lead, -1, hn, l)
            else:
                self.edge(lead, tabs=2)

        for j in range(hn):
            for i in range(n):
                if not (i % 2) ^ self.top:
                    self.rectangularHole(self.settings.spacing + 0.5 * t + p + i * (t + p), e + 2.5 * t, t, t)
            self.polyline(*poly)
            if j < (hn - 1):
                segment = (l - hn * width) / (hn - 1)
                if self._should_use_custom_spacing(segment):
                    self._hinge_spacing_segment(segment, j, hn, l)
                else:
                    self.edge(segment, tabs=2)

        if hn == 1:
            tail = (l - width) / 2
            if self._should_use_custom_spacing(tail):
                self._hinge_spacing_segment(tail, hn, hn, l)
            else:
                self.edge(tail, tabs=2)

    def parts(self, move=None) -> None:
        e, b = self.settings.eye, self.settings.bore
        t = self.settings.thickness

        n = self.settings.eyes_per_hinge * self.settings.hinges
        pairs = n // 2 + 2 * (n % 2)

        if self.settings.style == "outside":
            th = 2 * e + 4 * t
            tw = n * (max(3 * t, 2 * e) + self.boxes.spacing)
        else:
            th = 4 * e + 3 * t + self.boxes.spacing
            tw = max(e, 2 * t) * pairs

        if self.move(tw, th, move, True, label="hinges"):
            return

        if self.settings.style == "outside":
            ax = max(t / 2, e - t)
            self.moveTo(t + ax)
            for i in range(n):
                if self.angled:
                    if i > n // 2:
                        l = 4 * t + ax
                    else:
                        l = 5 * t + ax
                else:
                    l = 3 * t + e
                self.hole(0, e, b / 2.0)
                da = math.asin((t - ax) / e)
                dad = math.degrees(da)
                dy = e * (1 - math.cos(da))
                self.polyline(0, (180 - dad, e), 0, (-90 + dad), dy + l - e, (90, t))
                self.polyline(0, 90, t, -90, t, 90, t, 90, t, -90, t, -90, t,
                              90, t, 90, (ax + t) - e, -90, l - 3 * t, (90, e))
                self.moveTo(2 * max(e, 1.5 * t) + self.boxes.spacing)

            self.move(tw, th, move, label="hinges")
            return

        if e <= 2 * t:
            if self.angled:
                corner = [2 * e - t, (90, 2 * t - e), 0, -90, t, (90, e)]
            else:
                corner = [2 * e, (90, 2 * t)]
        else:
            a = math.asin(2 * t / e)
            ang = math.degrees(a)
            corner = [e * (1 - math.cos(a)) + 2 * t, -90 + ang, 0, (180 - ang, e)]
        self.moveTo(max(e, 2 * t))
        for i in range(n):
            self.hole(0, e, b / 2.0)
            self.corner(180,e)
            self.moveTo(0,0,-90)
            self.polyline(*[ t-self.burn, 90, t, -90, t, -90, t, 90, t, 90, t - self.burn, (90, t)] + corner)
            self.moveTo(self.boxes.spacing, 4 * e + 3 * t + self.boxes.spacing, 180)
            if i % 2:
                self.moveTo(2 * max(e, 2 * t) + 2 * self.boxes.spacing)

        self.move(th, tw, move, label="hinges")


class LatcheEdge(edges.BaseEdge):
    """Custom edge and parts for the toolbox latch (trava).

    - As an edge: currently behaves like a straight edge, keeping
      compatibility with existing walls while the latch is placed as a
      separate part.
    - As parts: renders the latch geometry directly.
    """

    char = "r"
    description = "Toolbox latch (edge + parts)"

    def __init__(self, boxes, settings=None, char_code: str = 'r') -> None:
        super().__init__(boxes, settings)
        # support two registrations from the same class: 'r' and 'R'
        if char_code in ('r', 'R'):
            self.char = char_code

    def __call__(self, length, **kw):
        # Draw latch-related holes repeated along this edge, then draw the edge line.
        b = self.boxes
        t = self.thickness
        count = max(0, int(getattr(b, "latche_count", 0) or 0))

        # Dimensions derived from the latch profile
        # Disc that passes through panel ("peça O"): radius r25, center y at r25
        r25 = 2.5 * t
        y_disc = r25

        if self.char == 'R':
            # Draw repeated rectangles at y = 1.5*t from the edge
            # height = t; width = sec(r25, mt*4.5 - (hl2/2), 0)/2 + cbc
            if count > 0 and length > 0:
                segment = length / float(count)
                mt = t
                r25 = 2.5 * mt
                hl2 = self._sec(4.5 * mt, r25, 0.0)
                # corner blend compensation (cbc): default small value,
                # but if inner corners use dogbone, follow user's dogbone size
                cbc = 0.11
                if getattr(b, "inner_corners", None) == "dogbone":
                    if getattr(b, "D", None):
                        cbc = b.D
                    else:
                        cbc = getattr(b, "R", cbc)
                rect_w = self._sec(r25, 4.5 * mt - (hl2 / 2.0), 0.0) / 2.0 + cbc
                rect_h = mt
                # distance from edge to lower rectangle edge = t -> center at 1.5*t
                y_center = 1.0 * mt + rect_h / 2.0
                for i in range(count):
                    cx = (i + 0.5) * segment
                    with self.saved_context():
                        self.moveTo(cx, 0)
                        # place rectangle so its left edge coincides with the previous center (x=0)
                        self.rectangularHole(-cbc, y_center, rect_w, rect_h, center_x=False)

            self.edge(length, tabs=2)
            return

        if count > 0 and length > 0:
            segment = length / float(count)
            dx = t
            # rotate the pair of centers by +45° around their midpoint
            offset = t / math.sqrt(2.0)
            # Place the LOWER diamond center at exactly 2.5*t from the edge.
            # Empirically, inner kerf compensation shifts the apparent center
            # by burn/√2. Compensate here to keep the measured distance exact.
            y_center = 2.5 * t + offset - (self.burn / math.sqrt(2.0))
            self.ctx.save()
            try:
                for i in range(count):
                    cx = (i + 0.5) * segment
                    # two diamonds (squares @45°) with inner corners that remain machinable
                    self._diamond_hole(cx - offset, y_center - offset, dx, angle=45)
                    self._diamond_hole(cx + offset, y_center + offset, dx, angle=45)
            finally:
                self.ctx.restore()

        # Keep the wall edge straight
        self.edge(length, tabs=2)

    def parts(self, move: str | None = None) -> None:
        self._render_latch(move=move or "", label="Latche")

    @holeCol
    def _diamond_hole(self, cx: float, cy: float, side: float, angle: float = 45.0):
        # Draw a rotated square hole while preserving -90 degree inner corners.
        s = side
        rad = math.radians(angle)
        start_x = cx + (-0.5 * s) * math.cos(rad)
        start_y = cy + (-0.5 * s) * math.sin(rad)

        self.moveTo(start_x, start_y, angle)
        self.edge(0.5 * s)
        for d in (s, s, s, 0.5 * s):
            self.corner(-90, 0)
            self.edge(d)

    # Helper used by the latch geometry
    def _sec(self, r: float, c: float, b_: float = 0.0) -> float:
        d = c - b_
        val = r * r - d * d
        if val < 0:
            val = 0.0
        return 2.0 * math.sqrt(val)

    def _render_latch(self, move: str, label: str) -> None:
        b = self.boxes
        mt = b.thickness
        spacing = b.spacing
        count = max(0, int(getattr(b, "latche_count", 0) or 0))
        # Draw as many latch assemblies as requested by ``latche_count``.
        if count <= 0:
            return
        r25 = 2.5 * mt
        rm = r25 - b.burn
        hl = self._sec(3.5 * mt, r25, 0.0)
        hl2 = self._sec(4.5 * mt, r25, 0.0)
        d1 = 4.5 * mt - (hl2 / 2.0)
        cbc = 0.11
        if b.inner_corners == "dogbone":
            cbc = b.D if getattr(b, "D", 0.0) else getattr(b, "R", cbc)
        height = 5.0 * mt + hl2 / 2.0
        width = 13.0 * mt + 2.0 * spacing
        segment = self._sec(r25, 4.5 * mt - (hl2 / 2.0), 0.0) / 2.0 + cbc
        stride = width + spacing
        total_width = width + (count - 1) * stride if count > 1 else width
        layout_label = label if count == 1 else f"{label} x{count}"

        if b.move(total_width, height, move, before=True, label=layout_label):
            return

        for idx in range(count):
            offset = idx * stride
            with b.saved_context():
                b.ctx.translate(offset, 0.0)
                self._draw_latch_assembly(mt, spacing, r25, rm, hl, hl2, d1, cbc, segment)

        b.ctx.stroke()
        b.move(total_width, height, move, label=layout_label)

    def _draw_latch_assembly(
        self,
        mt: float,
        spacing: float,
        r25: float,
        rm: float,
        hl: float,
        hl2: float,
        d1: float,
        cbc: float,
        segment: float,
    ) -> None:
        b = self.boxes
        burn = b.burn

        # Piece u
        b.moveTo(0, 0)
        b.polyline(mt, 90, mt, -90, mt, -90, mt, 90, mt, 90, mt * 2, 90, mt * 3, 90, mt * 2, 90)
        b.moveTo(0, mt * 2 + spacing)

        # Piece U
        b.polyline(mt * 3, 90, mt * 3, 90, mt, 90, mt * 2, -90, mt, -90, mt * 2, 90, mt, 90, mt * 3, 90)
        b.moveTo(mt * 3 + 2 * burn + spacing, 2 * burn + mt * 3)

        # Piece []
        b.polyline(segment, 90, mt * 2, 90, segment, 90, mt * 2, 90)
        b.moveTo(mt * 2.5, -mt * 5 - spacing)

        # Piece O
        b.circle(0, rm, rm)
        b.moveTo(-mt / 2.0, mt * 2, 90)
        b.polyline(
            0,
            90,
            mt,
            -90,
            mt,
            -90,
            mt,
            90,
            mt,
            -90,
            mt,
            -90,
            mt,
            90,
            mt,
            -90,
            mt,
            -90,
            mt,
            90,
            mt,
            -90,
            mt,
            -90,
            mt,
        )
        b.moveTo(0, 0, -90)
        b.moveTo(mt * 5.5 + burn + spacing, -mt * 2 - burn)

        # Locker
        b.polyline(0, [90, rm], hl / 2.0)
        b.moveTo(0, 0, self._arcsec(3.5 * mt, hl / 2.0, 0.0, True))
        b.polyline(0, [self._arcsec(3.5 * mt, hl / 2.0, 0.0, False), 3.5 * mt - burn])
        b.polyline(cbc, -90, mt + 2 * burn, -90, segment)
        b.moveTo(0, 0, 90 + self._arcsec(r25, d1, 0.0, True))
        b.polyline(0, [self._arcsec(r25, d1, 0.0, False), rm], 0, [90, rm], hl2 / 2.0, [90, rm])
        b.moveTo(0, mt * 2.5, 90)
        b.circle(0, 0, self._locker_radius(mt, mt * 3.0))
        b.moveTo(-mt * 2.5, -mt * 2.5)

    def _arcsec(self, radius: float, chord: float, base: float, use_sine: bool) -> float:
        quotient = (chord - base) / radius
        angle = math.asin(quotient) if use_sine else math.acos(quotient)
        return math.degrees(angle)

    def _locker_radius(self, x_val: float, y_val: float) -> float:
        return math.sqrt(x_val * x_val + y_val * y_val) / 2.0


class Handle:
    """Custom handle profile."""

    def __init__(self, boxes: Boxes, width: float, height: float, thickness: float, gap: float) -> None:
        self.boxes = boxes
        self.width = width
        self.height = height
        self.thickness = thickness
        self.gap = gap
        self.burn = boxes.burn

    def render(self, move: str = "", label: str = "Handle") -> None:
        burn = self.burn
        h = self.height
        w = self.width
        t = self.thickness
        g = self.gap
        cr = 0.5 + burn

        if self.width <= 0 or self.height <= 0:
            return

        b = self.boxes
        if b.move(self.width, self.height, move, before=True, label=label):
            return

        positive_char_getter = getattr(b, "_positive_finger_edge_char", None)
        finger_char = positive_char_getter() if callable(positive_char_getter) else 'f'
        finger_edge = b.edges.get(finger_char) or b.edges.get('f')
        if finger_edge is None:
            return
        b.moveTo(0, 0)
        b.polyline(w-t,[90, (t / 2) - burn],h-t/2,[90,0])
        finger_edge(t)
        b.corner(90,0)
        b.polyline(g-cr,[-90,cr],w - 2*(t+burn+cr),[-90,cr],g-cr,90)
        finger_edge(t)
        b.corner(90,0)
        b.polyline(h-t/2,[90,(t / 2) - burn])
        b.ctx.stroke()

        b.move(self.width, self.height, move, label=label)


class ToolBox(Boxes):
    """Finger jointed toolbox with four walls and a bottom panel."""

    ui_group = "Box"

    description = """A straightforward rectangular toolbox that generates four
walls and a bottom panel using finger joints. Dimensions can be provided either
as internal or external measurements."""

    def __init__(self) -> None:
        Boxes.__init__(self)
        self.addSettingsArgs(edges.FingerJointSettings)
        self.addSettingsArgs(edges.CabinetHingeSettings)

        self.buildArgParser("x", "y", "h", "outside")
        self.argparser.add_argument(
            "--custom_spacing",
            action="store",
            type=float,
            default=None,
            help="override spacing between parts (in mm)")
        self.argparser.add_argument(
            "--handle",
            action="store",
            type=bool,
            default=True,
            help="gera a peca de alca (True/False)")
        self.argparser.add_argument(
            "--handle_height",
            action="store",
            type=float,
            default=70,
            help="altura total da alca em mm")
        self.argparser.add_argument(
            "--handle_width",
            action="store",
            type=float,
            default=100,
            help="largura total da alca em mm")
        self.argparser.add_argument(
            "--handle_thickness",
            action="store",
            type=float,
            default=30,
            help="espessura (largura do perfil) da alca em mm")
        self.argparser.add_argument(
            "--handle_gap",
            action="store",
            type=float,
            default=30,
            help="abertura central da alca (gap) em mm")
        self.argparser.add_argument(
            "--divider_hole_offset",
            action="store",
            type=float,
            default=None,
            help="deslocamento vertical (em mm) dos furos do divisor no fundo")
        self.argparser.add_argument(
            "--divider_clearance",
            action="store",
            type=float,
            default=None,
            help="folga adicional (em mm) aplicada nas dimensoes do divisor")
        self.argparser.add_argument(
            "--latche_count",
            action="store",
            type=int,
            default=0,
            help="quantidade de travas (latches) distribuídas ao longo da borda 'r'")

    def open(self) -> None:
        super().open()
        self._override_cabinet_hinge_edges()
        self._register_custom_finger_edges()
        self._register_latche_edge()
    
    def _override_cabinet_hinge_edges(self) -> None:
        base_edge = self.edges.get('u')
        if base_edge is None:
            return
        settings = base_edge.settings
        for top, angled in ((False, False), (True, False), (False, True), (True, True)):
            edge = CustomCabinetHingeEdge(self, settings, top=top, angled=angled)
            self.addPart(edge)

    def _register_custom_finger_edges(self) -> None:
        base_edge = self.edges.get('f')
        if base_edge is None:
            return
        positive_play_edge = FingerJointPositivePlayEdge(self, base_edge.settings)
        divider_edge = FingerJointDividerEdge(self, base_edge.settings)
        self.addPart(positive_play_edge)
        self.addPart(divider_edge)

    def _register_latche_edge(self) -> None:
        # Register the custom latch edge/parts under a dedicated character.
        self.addPart(LatcheEdge(self, char_code='r'))
        self.addPart(LatcheEdge(self, char_code='R'))

    def _positive_finger_edge_char(self) -> str:
        return 'x' if self.edges.get('x') is not None else 'f'

    def render(self) -> None:

        x, y, h = self.x, self.y, self.h

        if self.outside:
            x = self.adjustSize(x)
            y = self.adjustSize(y)
            h = self.adjustSize(h)

        material_thickness = self.thickness
        spacing = self.custom_spacing if self.custom_spacing is not None else self.spacing
        if self.custom_spacing is not None:
            self.spacing = spacing
        half_height = h / 2
        move_spacing = (2 * material_thickness) + spacing
        handle_width = self.handle_width
        handle_height = self.handle_height
        handle_thickness = self.handle_thickness
        handle_gap = self.handle_gap
        divider_clearance = self.divider_clearance
        if divider_clearance is None:
            divider_clearance = max(0.0, self.burn)
        divider_width = x - 2 * divider_clearance
        divider_height = h - 2 * divider_clearance
        if divider_width <= 0:
            divider_width = x
        if divider_height <= 0:
            divider_height = h
        positive_edge_char = self._positive_finger_edge_char()

        self.rectangularWall(x, half_height, "FFuF", move="right", label="Lower Wall 1")
        self.rectangularWall(y, half_height, "Ffef", move="up", label="Lower Wall 2")
        self.rectangularWall(x, half_height, "FFrF", move="left right", label="Lower Wall 3")
        self.rectangularWall(y, half_height, "Ffef", move="up", label="Lower Wall 4") 

        self.moveTo(-(x + move_spacing), 0) 

        self._render_bottom_panel(x, y, positive_edge_char)
        self.rectangularWall(x, y, positive_edge_char * 4, move="up", label="Top")

        self.moveTo(-(x + move_spacing), 0)

        self.rectangularWall(x, half_height, "UFFF", move="right", label="Upper Wall 1")
        self.rectangularWall(y, half_height, "efFf", move="up", label="Upper Wall 2")
        self.rectangularWall(x, half_height, "RFFF", move="left right", label="Upper Wall 3")
        self.rectangularWall(y, half_height, "efFf", move="up", label="Upper Wall 4")

        stacked_height = 2 * half_height + y + 3 * spacing
        self.moveTo(-x-2*spacing, 0)

        # Shrink divider slightly on both axes to keep it from binding.
        divider_edges = f"{positive_edge_char}eee"
        self.rectangularWall(divider_width, divider_height, divider_edges, move="right", label="Divider")
        self.moveTo(3* spacing)

        handle_piece = Handle(self, width=handle_width, height=handle_height, thickness=handle_thickness, gap=handle_gap)
        handle_piece.render(move="right", label="Handle")
        self.edges['u'].parts(move="right right right")

        # Render the latch using the custom LatcheEdge parts.
        latche_edge = self.edges.get('r')
        if latche_edge is not None:
            latche_edge.parts()

    def _render_bottom_panel(self, width: float, depth: float, positive_char: str) -> None:
        """Render bottom panel and include divider slots if the custom edge exists."""
        fallback_char = positive_char or 'f'
        edge_char = "w" if self.edges.get('w') is not None else fallback_char
        edges_string = edge_char + fallback_char * 3
        self.rectangularWall(width, depth, edges_string, move="right", label="Bottom")
