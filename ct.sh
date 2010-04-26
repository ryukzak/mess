mkdir log
erl -make
run_test -config "./conf/ct.cfg" -dir ebin -logdir log $1
