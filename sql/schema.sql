CREATE TABLE "users"
(
    id SERIAL PRIMARY KEY,
    username VARCHAR(50) NOT NULL UNIQUE,
    password VARCHAR NOT NULL,
    created_at TIMESTAMP DEFAULT NOW()
);

CREATE TABLE "spellcheck"
(
    id SERIAL PRIMARY KEY,
    data JSONB
);

CREATE TABLE "phrases"
(
    id SERIAL PRIMARY KEY,
    text TEXT NOT NULL,
    author_id INTEGER NOT NULL REFERENCES users,
    spellcheck_id INTEGER REFERENCES spellcheck,
    is_open BOOLEAN NOT NULL DEFAULT TRUE,
	chosen_alt_id INTEGER,
    created_at TIMESTAMP DEFAULT NOW()
);

CREATE TABLE "alternatives"
(
    id SERIAL PRIMARY KEY,
    phrase_id INTEGER NOT NULL REFERENCES phrases,
    text TEXT NOT NULL,
    author_id INTEGER NOT NULL REFERENCES users,
    spellcheck_id INTEGER REFERENCES spellcheck,
    created_at TIMESTAMP DEFAULT NOW()
);

ALTER TABLE phrases
	ADD FOREIGN KEY (chosen_alt_id)
	REFERENCES alternatives (id)
	DEFERRABLE INITIALLY DEFERRED;
