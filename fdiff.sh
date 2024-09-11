#!/bin/bash

set -x

#src=base.dev
#dst=base

#src=base.dev
#dst=base

src=rest.dev
dst=rest

#src=base.dev
#dst=rest.dev

#src=base
#dst=rest

cmp ${src}/fort.911  ${dst}/fort.911
cmp ${src}/fort.912  ${dst}/fort.912
cmp ${src}/fort.913  ${dst}/fort.913
cmp ${src}/fort.914  ${dst}/fort.914
cmp ${src}/fort.915  ${dst}/fort.915
echo
cmp ${src}/fort.921  ${dst}/fort.921
cmp ${src}/fort.922  ${dst}/fort.922
cmp ${src}/fort.923  ${dst}/fort.923
cmp ${src}/fort.924  ${dst}/fort.924
cmp ${src}/fort.923  ${dst}/fort.925
echo
cmp ${src}/fort.1011  ${dst}/fort.1011
cmp ${src}/fort.1012  ${dst}/fort.1012
cmp ${src}/fort.1013  ${dst}/fort.1013
cmp ${src}/fort.1014  ${dst}/fort.1014
cmp ${src}/fort.1015  ${dst}/fort.1015
echo
cmp ${src}/fort.1021  ${dst}/fort.1021
cmp ${src}/fort.1022  ${dst}/fort.1022
cmp ${src}/fort.1023  ${dst}/fort.1023
cmp ${src}/fort.1024  ${dst}/fort.1024
cmp ${src}/fort.1025  ${dst}/fort.1025
echo
cmp ${src}/fort.1031  ${dst}/fort.1031
cmp ${src}/fort.1032  ${dst}/fort.1032
cmp ${src}/fort.1033  ${dst}/fort.1033
cmp ${src}/fort.1034  ${dst}/fort.1034
cmp ${src}/fort.1035  ${dst}/fort.1035

echo
echo

cmp ${src}/fort.2011  ${dst}/fort.2011
cmp ${src}/fort.2012  ${dst}/fort.2012
cmp ${src}/fort.2013  ${dst}/fort.2013
cmp ${src}/fort.2014  ${dst}/fort.2014
cmp ${src}/fort.2015  ${dst}/fort.2015
echo
cmp ${src}/fort.2021  ${dst}/fort.2021
cmp ${src}/fort.2022  ${dst}/fort.2022
cmp ${src}/fort.2023  ${dst}/fort.2023
cmp ${src}/fort.2024  ${dst}/fort.2024
cmp ${src}/fort.2025  ${dst}/fort.2025
echo
cmp ${src}/fort.2031  ${dst}/fort.2031
cmp ${src}/fort.2032  ${dst}/fort.2032
cmp ${src}/fort.2033  ${dst}/fort.2033
cmp ${src}/fort.2034  ${dst}/fort.2034
cmp ${src}/fort.2035  ${dst}/fort.2035
