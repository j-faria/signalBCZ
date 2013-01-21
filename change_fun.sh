#!/bin/sh
NEW=fun2
OLD=fun

# the command is
# perl -pi -w -e 's/fun2/fun/g;' get_chi_square.f

echo "perl -pi -w -e 's/${OLD}/${NEW}/g;' sig_bcz/get_chi_square.f"
echo "perl -pi -w -e 's/${OLD}/${NEW}/g;' sig_bcz/writeout2.f"
echo "perl -pi -w -e 's/${OLD}\(/${NEW}\(/g;' common/sec.f"
echo "perl -pi -w -e 's/${OLD}\(/${NEW}\(/g;' common/funk.f"
echo "perl -pi -w -e 's/${OLD}.o/${NEW}.o/g;' sig_bcz/Makefile"
