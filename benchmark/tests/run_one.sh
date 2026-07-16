#!/bin/bash
../benchmark-util/benchmark -mlton '../../build/bin/mlton ' -mlton '../../build/bin/mlton -pre-flatten-max-iters 1 -pre-flatten-consumer-policy always -pre-flatten-resolve-policy local -pre-flatten-types-policy tuple -pre-flatten-recursive-steps 10 -pre-flatten-phase late -pre-flatten-transfer-policy tail_only'  barnes-hut
