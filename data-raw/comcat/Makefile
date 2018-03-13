WS=01.fdsnws
CATR=02.merge_query_chunks.R
COMCAT=comcat_catalog.csv
CHUNKS=query_chunk_01.csv query_chunk_02.csv

all: $(COMCAT)

$(COMCAT): $(CHUNKS) $(CATR)
	Rscript $(CATR)

$(CHUNKS): $(WS)
	$(WS)

refresh: clean all

clean:
	rm -f query_chunk_??.csv
	rm -f comcat_catalog.csv
