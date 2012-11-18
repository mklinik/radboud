CLEAN_FLAGS=-I ~/clean-2.4/lib/StdLib -nt -lt

CLEAN_FLAGS_GAST=-I ~/clean-2.4/lib/StdLib -I ~/clean-2.4/lib/Gast -I ~/clean-2.4/lib/MersenneTwister -I ~/clean-2.4/lib/Generics -nt
assignment07klinik: assignment07klinik.icl
	clm ${CLEAN_FLAGS_GAST} $@ -o $@

assignment08klinik: assignment08klinik.icl
	clm ${CLEAN_FLAGS_GAST} -h 10M -b $@ -o $@

%: %.icl
	clm ${CLEAN_FLAGS} $@ -o $@
