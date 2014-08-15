for r in `seq -w 02 18`; do
    echo "java -Dconfig.file=word-deid.conf -Dorg.whym.growthring.repeats=$r -jar main.jar < deid_t2_v2_unlabeled.txt > data/deid_t2_v2_mr-w$r.txt";
done
