"""Watcher simples para executar `toolbox_runner.py` quando `toolbox.py` for salvo.

Uso:
    python watch_toolbox.py

O script permanece em execução monitorando o arquivo alvo. Ao detectar uma
mudança no timestamp de modificação, executa o runner. Pressione Ctrl+C para
encerrar.
"""

from __future__ import annotations

import subprocess
import sys
import time
from pathlib import Path


ROOT = Path(__file__).resolve().parent
TOOLBOX_FILE = ROOT / "boxes" / "generators" / "toolbox.py"
RUNNER_SCRIPT = ROOT / "toolbox_runner.py"


def _run_runner() -> None:
    """Executa o runner e repassa o código de saída."""
    command = [sys.executable, str(RUNNER_SCRIPT)]
    print(f"[watcher] Executando: {' '.join(command)}")
    result = subprocess.run(command, cwd=ROOT)
    if result.returncode == 0:
        print("[watcher] Execução concluída com sucesso.\n")
    else:
        print(f"[watcher] Execução terminou com código {result.returncode}.\n")


def main() -> None:
    if not TOOLBOX_FILE.exists():
        print(f"[watcher] Arquivo alvo não encontrado: {TOOLBOX_FILE}")
        sys.exit(1)
    if not RUNNER_SCRIPT.exists():
        print(f"[watcher] Runner não encontrado: {RUNNER_SCRIPT}")
        sys.exit(1)

    print(f"[watcher] Monitorando {TOOLBOX_FILE}")
    print("[watcher] Pressione Ctrl+C para encerrar.")

    try:
        last_mtime = TOOLBOX_FILE.stat().st_mtime_ns
    except OSError as exc:
        print(f"[watcher] Erro ao obter timestamp inicial: {exc}")
        sys.exit(1)

    try:
        while True:
            time.sleep(0.5)
            try:
                current_mtime = TOOLBOX_FILE.stat().st_mtime_ns
            except OSError:
                continue
            if current_mtime != last_mtime:
                last_mtime = current_mtime
                # Curta espera para evitar executar durante escrita parcial.
                time.sleep(0.2)
                _run_runner()
    except KeyboardInterrupt:
        print("\n[watcher] Encerrando monitoramento.")


if __name__ == "__main__":
    main()
