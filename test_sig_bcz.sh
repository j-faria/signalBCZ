echo "/home/joao/Documents/FCUP/TESE/bcz_fit/mesa_bestA.freq" | ./sig_bcz/sig_bcz_run /home/joao/Documents/FCUP/TESE/bcz_fit/test_results/sig_bcz_input > results.txt

rm res cof sig
rm fort.7

diff results.txt /home/joao/Documents/FCUP/TESE/bcz_fit/test_results/mesa.a.res
