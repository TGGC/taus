override CAFLAGS += -g
LDFLAGS =
VPATH = build

build:
	mkdir build

build/%.o: %.s Makefile | build
	ca65 $(CAFLAGS) --create-dep $@.d $< -o $@

build/%: %.cfg
	ld65 $(LDFLAGS) -Ln $(basename $@).lbl --dbgfile $(basename $@).dbg -o $@ -C $< $(filter %.o,$^)

build/%.ips.cfg: ips-segments.awk
	od65 --dump-options $(filter %.o,$^) | grep '"ips:' | sed -E 's/^ +Data: +"ips: (.*)"$$/\1/' | sort -n | awk -f ips-segments.awk > $@

build/%.nes: build/%.ips
	# Second prerequisite is assumed to be a .nes source
	# If the first time fails, run it a second time to display output
	flips --apply $< $(word 2,$^) $@ > /dev/null || flips --apply $< $(word 2,$^) $@
	flips --create $(word 2,$^) $@ build/$*.dist.ips > /dev/null

build/%: %.ips
	# Second prerequisite is assumed to be source
	# If the first time fails, run it a second time to display output
	flips --apply $< $(word 2,$^) $@ > /dev/null || flips --apply $< $(word 2,$^) $@
	flips --create $(word 2,$^) $@ build/$*.dist.ips > /dev/null

build/%.chrs/fake: %.chr | build
	[ -d build/$*.chrs ] || mkdir build/$*.chrs
	touch $@
	split -d -b 16 $< build/$*.chrs/
	mv build/$*.chrs/10 build/$*.chrs/0a
	mv build/$*.chrs/11 build/$*.chrs/0b
	mv build/$*.chrs/12 build/$*.chrs/0c
	mv build/$*.chrs/13 build/$*.chrs/0d
	mv build/$*.chrs/14 build/$*.chrs/0e
	mv build/$*.chrs/15 build/$*.chrs/0f
	mv build/$*.chrs/16 build/$*.chrs/10
	mv build/$*.chrs/17 build/$*.chrs/11
	mv build/$*.chrs/18 build/$*.chrs/12
	mv build/$*.chrs/19 build/$*.chrs/13
	mv build/$*.chrs/20 build/$*.chrs/14
	mv build/$*.chrs/21 build/$*.chrs/15
	mv build/$*.chrs/22 build/$*.chrs/16
	mv build/$*.chrs/23 build/$*.chrs/17
	mv build/$*.chrs/24 build/$*.chrs/18
	mv build/$*.chrs/25 build/$*.chrs/19
	mv build/$*.chrs/26 build/$*.chrs/1a
	mv build/$*.chrs/27 build/$*.chrs/1b
	mv build/$*.chrs/28 build/$*.chrs/1c
	mv build/$*.chrs/29 build/$*.chrs/1d
	mv build/$*.chrs/30 build/$*.chrs/1e
	mv build/$*.chrs/31 build/$*.chrs/1f
	mv build/$*.chrs/32 build/$*.chrs/20
	mv build/$*.chrs/33 build/$*.chrs/21
	mv build/$*.chrs/34 build/$*.chrs/22
	mv build/$*.chrs/35 build/$*.chrs/23
	mv build/$*.chrs/36 build/$*.chrs/24
	mv build/$*.chrs/37 build/$*.chrs/25
	mv build/$*.chrs/38 build/$*.chrs/26
	mv build/$*.chrs/39 build/$*.chrs/27
	mv build/$*.chrs/40 build/$*.chrs/28
	mv build/$*.chrs/41 build/$*.chrs/29
	mv build/$*.chrs/42 build/$*.chrs/2a
	mv build/$*.chrs/43 build/$*.chrs/2b
	mv build/$*.chrs/44 build/$*.chrs/2c
	mv build/$*.chrs/45 build/$*.chrs/2d
	mv build/$*.chrs/46 build/$*.chrs/2e
	mv build/$*.chrs/47 build/$*.chrs/2f
build/%.rle: % rle-enc.awk | build
	# 'basenc --base16 -w2' and 'basenc --base16 -d' would also work, but
	# basenc isn't as widely available as xxd since it was added in
	# coreutils 8.31
	xxd -c1 -p $< | LC_ALL=C awk -f rle-enc.awk | xxd -r -p > $@

build/%.s: %.bin %.info Makefile | build
	# Strip off the first two lines of header, which contain variable
	# information; they cause merge conflicts
	da65 -i $(word 2,$^) $< | tail -n +3 > $@
	da65 -i $(word 2,$^) --comments 2 $< > $(basename $@).v2.s

clean:
	[ ! -d build/ ] || rm -r build/

.PHONY: clean

.SUFFIXES:

ifneq "$(V)" "1"
.SILENT:
endif

include $(wildcard build/*.d)

.SECONDEXPANSION:
build/%: %.diff $$(wildcard build/diffhead-$$*)
	# Last prerequisite is assumed to be basefile
	###
	# Sync diffhead and diff for manual edits
	if [ build/diffhead-$* -nt $@ ]; then \
		diff -u --label orig --label mod -U 5 -F : build/diffbase-$* build/diffhead-$* > $@.tmp \
			|| [ $$? -eq 1 ] && mv $@.tmp $<; \
	elif [ $< -nt $@ -a -e build/diffbase-$* ]; then \
		cp build/diffbase-$* $@.tmp && patch -s $@.tmp $< && mv $@.tmp build/diffhead-$*; \
	fi
	# Now do build-triggered updates
	if [ ! -e build/diffbase-$* -o $(word $(words $^),$^) -nt build/diffbase-$* ]; then \
		cp $(word $(words $^),$^) $@.tmpbase && \
		cp $(word $(words $^),$^) $@.tmphead && patch -s $@.tmphead $< && \
		mv $@.tmpbase build/diffbase-$* && mv $@.tmphead build/diffhead-$*; \
	fi
	echo "; DO NOT MODIFY. Modify diffhead-$* instead" | cat - build/diffhead-$* > $@
