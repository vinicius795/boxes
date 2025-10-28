"""Geracao centralizada para o ToolBox.

Edite o dicionario `TOOLBOX_CONFIG` abaixo para ajustar apenas os valores que
voce quer alterar. Ao executar `python toolbox_runner.py`, o script mostra todos
os parametros disponiveis, destaca os valores personalizados e chama o gerador
`ToolBox` com esses ajustes.
"""

from __future__ import annotations

from pathlib import Path
from typing import Any, Dict, Iterable

from boxes.generators.toolbox import ToolBox
from boxes.scripts import boxes_main

format = "svg"
# Ajuste aqui os valores que deseja sobrescrever.
# Somente os campos diferentes do padrao serao enviados ao gerador.
TOOLBOX_CONFIG: Dict[str, Any] = {
    # Globais (Boxes)
    "thickness": 6.0,
    "output": r"constructor." + format,
    "format": format,
    "tabs": 0.0,
    "qr_code": False,
    "debug": False,
    "labels": False,
    "reference": 100.0,
    "verbose": False,
    "inner_corners": "dogbone",
    "R": None,
    "D": 2.0,
    "burn": 0.1,
    # Especificos do ToolBox
    "x": 335.0,
    "y": 235.0,
    "h": 150.0,
    "outside": False,
    "custom_spacing": 10.2,
    "handle": True,
    "handle_height": 110.0,
    "handle_width": 210.0,
    "handle_thickness": 60.0,
    "handle_gap": 50.0,
    "divider_hole_offset": 60.0,
    "divider_clearance": 5.0,
    # Configuracoes FingerJoint (multiplicadores da espessura onde indicado)
    "FingerJoint_style": "rectangular",
    "FingerJoint_surroundingspaces": 2.0,
    "FingerJoint_space": 2.0,
    "FingerJoint_finger": 2.0,
    "FingerJoint_width": 1.0,
    "FingerJoint_edge_width": 1.0,
    "FingerJoint_play": 0.1,
    "FingerJoint_extra_length": 0.0,
    "FingerJoint_bottom_lip": 0.0,
    # Configuracoes CabinetHinge (multiplicadores da espessura onde indicado)
    "CabinetHinge_bore": 3.2,
    "CabinetHinge_eyes_per_hinge": 5,
    "CabinetHinge_hinges": 2,
    "CabinetHinge_style": "inside",
    "CabinetHinge_eye": 1.5,
    "CabinetHinge_play": 0.1,
    "CabinetHinge_spacing": 2.0,
}


def _ascii_only(text: str) -> str:
    """Remove caracteres fora do ASCII para evitar erros de console no Windows."""
    return text.encode("ascii", "ignore").decode("ascii")


def _collect_parser_info() -> Dict[str, tuple[Any, str]]:
    """Retorna {dest: (valor_padrao, ajuda)} para todos os argumentos."""
    generator = ToolBox()
    parser = generator.argparser
    data: Dict[str, tuple[Any, str]] = {}

    for action in parser._actions:
        if not action.option_strings or action.dest == "help":
            continue
        default = parser.get_default(action.dest)
        help_text = (action.help or "").replace("\n", " ").strip()
        help_text = _ascii_only(help_text)
        data[action.dest] = (default, help_text)

    return data


def _format_cli_arg(name: str, value: Any) -> str:
    """Formata um parametro no estilo CLI (`--nome=valor`)."""
    if isinstance(value, bool):
        value_str = "True" if value else "False"
    else:
        value_str = str(value)
    return f"--{name}={value_str}"


def _build_cli_args(
    config: Dict[str, Any],
    defaults: Dict[str, tuple[Any, str]],
) -> Iterable[str]:
    """Gera os argumentos que serao passados ao gerador."""
    for name, value in config.items():
        if name == "verbose":
            continue
        if name not in defaults:
            raise ValueError(f"Parametro desconhecido em TOOLBOX_CONFIG: {name}")
        if value is None:
            continue
        if isinstance(value, Path):
            value = str(value)
        default_value = defaults[name][0]
        if value == default_value:
            continue
        yield _format_cli_arg(name, value)


def main() -> None:
    defaults = _collect_parser_info()

    cli_args = list(_build_cli_args(TOOLBOX_CONFIG, defaults))

    verbose = bool(TOOLBOX_CONFIG.get("verbose", False))
    if verbose:
        print("Parametros disponiveis para o gerador ToolBox:")
        for name in sorted(defaults):
            default_value, help_text = defaults[name]
            if help_text:
                print(f"  --{name} (padrao: {default_value!r}) -> {help_text}")
            else:
                print(f"  --{name} (padrao: {default_value!r})")

        if cli_args:
            print("\nUsando os seguintes ajustes personalizados:")
            for arg in cli_args:
                print(f"  {arg}")
        else:
            print("\nNenhum ajuste personalizado em TOOLBOX_CONFIG; usando apenas os padroes.")

        print("\nComando equivalente:")
        command = ["python", "-m", "boxes.scripts.boxes_main", "--generator", "ToolBox", *cli_args]
        print("  " + " ".join(command))

    boxes_main.run_generator("ToolBox", cli_args)

    if verbose:
        destino = TOOLBOX_CONFIG.get("output", defaults["output"][0])
        print(f"\nGeracao concluida. Arquivo salvo em: {destino}")


if __name__ == "__main__":
    main()
