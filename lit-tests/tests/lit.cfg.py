import lit.formats
import pathlib
import os

# Name of the test suite
config.name = 'MLton LIT Tests'

# File extensions to treat as test files
config.suffixes = ['.sml']

# The test format to use. ShTest is the standard format that supports
# RUN: lines in the test files.
config.test_format = lit.formats.ShTest(True)

# The root path where tests are located
config.test_source_root = os.path.dirname(__file__)

# Base paths to tools
SOURCE_ROOT = pathlib.Path(config.test_source_root)
TOOLS_ROOT = SOURCE_ROOT.parent / 'tools'
OUTPUT_ROOT = SOURCE_ROOT.parent / 'output'

# Add wrapper scripts for MLton compile, print-c, and run
MLTON_RUN_TOOL = str(TOOLS_ROOT / 'mlton-run.sh')
config.substitutions.append(('mlton-run', MLTON_RUN_TOOL))

MLTON_PRINT_C_TOOL = str(TOOLS_ROOT / 'mlton-print-c.sh')
config.substitutions.append(('mlton-print-c', MLTON_PRINT_C_TOOL))

MLTON_COMPILE_TOOL = str(TOOLS_ROOT / 'mlton-compile.sh')
config.substitutions.append(('mlton-compile', MLTON_COMPILE_TOOL))

# Map mpl- tools to mlton- tools for ported tests
config.substitutions.append(('mpl-run', MLTON_RUN_TOOL))
config.substitutions.append(('mpl-print-c', MLTON_PRINT_C_TOOL))
config.substitutions.append(('mpl-compile', MLTON_COMPILE_TOOL))

# Check if the user provided a custom build directory via --param
user_build_dir = lit_config.params.get('build_dir', None)
if user_build_dir:
    config.test_exec_root = os.path.abspath(user_build_dir)
else:
    config.test_exec_root = str(OUTPUT_ROOT)
