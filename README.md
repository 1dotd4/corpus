# Corpus

A tool for indexing and searching PDFs.

Special thanks to friends and lainons.

### TODO

- outline all requirements
- outline generic algorithm dataflow
- model data based on needs
- implement database funcionalities
...

### Requirements

- `pdftotext` (`poppler-utils`)

### Notes

`pdftotext` appends `^L` at the end of every page.
This can be used to point the page of the best result and other results.

Indexing:
- Take all documents and build all frequencies we need.
- Compose the frequencies to get weights for each term for each document.
- Frequencies are indexed by term. 
- Saved to db

Querying:
- Query is weighted and compose a query vector.
- Frequencies retrieved and compose a document vector.
- Calculate cos(θ) for each document and rank it (highest is best).
- Display results in a 20 result per page.
- Cache results.

Caching:
- Cache is invalidated when a new document is inserted or removed from the library.

## Resources

- [tf-idf](https://en.m.wikipedia.org/wiki/Tf%E2%80%93idf)
- [vsm](https://en.m.wikipedia.org/wiki/Vector_space_model)
- [stemming](https://snowballstem.org/algorithms/)

