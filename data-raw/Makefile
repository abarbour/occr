RDA=Dly1012d.rda
DLY=Dly1012d.zip
DLYSCP=01.get_Dly1012d

all: update

update: reup $(RDA)

reup:
	$(DLYSCP)

$(RDA): Dly1012d.R $(DLY)
	Rscript $<

$(DLY): $(DLYSCP)
	$<
