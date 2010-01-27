t1:
	ghc --make -O2 sample.hs

t2:
	ghc --make -O2 sample.hs -prof -auto-all -caf-all -fforce-recomp



r1:
	./sample 1e6 +RTS -sstderr


r2:
	./sample 1e6 +RTS -p -K100M
