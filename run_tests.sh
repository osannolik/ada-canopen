#!/bin/bash

gprbuild -P tests.gpr -f
./tests/bin/test_main

