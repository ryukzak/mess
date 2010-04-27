mkdir log 2> /dev/null
echo "============================================================"
erl -make
echo "============================================================"
run_test -config "./conf/ct.cfg" -dir ebin -logdir log $1
