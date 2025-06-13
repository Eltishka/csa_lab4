import contextlib
import io
import logging
import os
import tempfile

import machine
import pytest
import translator

MAX_LOG = 400000


@pytest.mark.golden_test("golden/*.yml")
def test_translator_and_machine(golden, caplog):

    caplog.set_level(logging.DEBUG)

    with tempfile.TemporaryDirectory() as tmpdirname:
        source = os.path.join(tmpdirname, "source.bf")
        input_stream = os.path.join(tmpdirname, "input.txt")
        target = os.path.join(tmpdirname, "target.bin")
        target_hex = os.path.join(tmpdirname, "target.bin.hex")
        inp = golden["in_stdin"]
        if golden["null_terminated_in"] == 1:
            inp += "\0"
        with open(source, "w", encoding="utf-8") as file:
            file.write(golden["in_source"])
        with open(input_stream, "w", encoding="utf-8") as file:
            file.write(inp)
        with contextlib.redirect_stdout(io.StringIO()) as stdout:
            translator.main(source, target)
            print("============================================================")
            machine.main(target, input_stream, golden["char_io"])

        with open(target, "rb") as file:
            file.read()
        with open(target_hex, encoding="utf-8") as file:
            code_hex = file.read()
        assert code_hex == golden.out["out_code_hex"]
        assert stdout.getvalue().strip() == golden.out["out_stdout"].strip()
        assert caplog.text == golden.out["out_log"]
