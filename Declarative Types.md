In most general terms, a *declarative type* is a bracketed list containing 
- the name of the type
- a list of fields

I'll use the following format:

`⟨dtypename f1 f2 f3 ...⟩`

The brackets are Unicode symbols.

To signify a *list* of fields, we use `⟨\*...⟩` notation.  The *list* is of variable length.  The items of the *list* are ordered. 

`⟨*dtypename f1 f2 f3 ...⟩`

The *list* is not a set, but an ordered sequence.  The *list* might contain duplicate entries.

The underlying code in some toolbox language knows that this is a list and uses appropriate methods for indexing the elements of the list, e.g. `first` and `rest`.  The toolbox code can choose to represent the list as an indexable array for efficiency (also known as a CDR-list).

Commas are whitespace.  Spaces are good enough to separate fields.  If commas are used, they act like spaces.

Each field is one field atom:
- string literal
- number literal
- \_
- dtype
- symbol - quoted symbol
- @symbol - evaluated symbol