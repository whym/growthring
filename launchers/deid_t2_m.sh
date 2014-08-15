for r in `seq -w 02 18`; do
    for m in `seq -w 01 15`; do
        echo "java -Dconfig.file=sais-deid.conf -Dorg.whym.growthring.minLen=$m -Dorg.whym.growthring.repeats=$r -jar main.jar < deid_t2_v2_unlabeled.txt > data/deid_t2_v2_mr-m$m-r$r.txt";
    done;
done
