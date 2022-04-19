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

### Tf-idf PoC results

The following interfaces are required:
- `insert-document`
- `insert-term`
- `remove-document`
- `remove-term`

To facilitate this, the data-structure must have:
- the number of documents;
- for each term, the number of documents it appers in;
- for each document, each term frequencies.

This is sufficient to allow:
- indexing new documents without evaluating every other document;
- querying a vector of terms and comparing the affinity.

### Vector space models PoC results

It is sufficent to map the query terms into a vector of weights based on tf-idf.
Then transposing the matrix and applying the cosine theorem we have the affinity of the documents based on the the query.
The ranking result should keep track of the documents while sorting.

### Datastructures notes

Depending on search method we can choose between some datastructures to count occurrencies of terms:
- hashmap are good when we lookup for exact terms;
- I don't know how good could be the SQLite Index;
- [DAFSA](https://en.m.wikipedia.org/wiki/Deterministic_acyclic_finite_state_automaton) are good for building a fuzzable search for each term (see lunr.js [TokenSet](https://lunrjs.com/docs/token_set.js.html)).

### SQLite notes

No. Using sqlite to update the term count is simply painful. It takes more than a minute to add a document because of this.

## Resources

- [tf-idf](https://en.m.wikipedia.org/wiki/Tf%E2%80%93idf)
- [vsm](https://en.m.wikipedia.org/wiki/Vector_space_model)
- [stemming](https://snowballstem.org/algorithms/)

