CREATE TABLE "users"
(
    id SERIAL PRIMARY KEY,
    username VARCHAR(50) NOT NULL UNIQUE,
    password VARCHAR NOT NULL,
    created_at TIMESTAMP DEFAULT NOW()
);

CREATE TABLE "phrases"
(
    id SERIAL PRIMARY KEY,
    text TEXT NOT NULL,
    user_id INTEGER NOT NULL REFERENCES users,
	chosen_alt INTEGER,
    created_at TIMESTAMP DEFAULT NOW()
);

CREATE TABLE "alternatives"
(
    id SERIAL PRIMARY KEY,
    phrase_id INTEGER NOT NULL REFERENCES phrases,
    text TEXT NOT NULL,
    user_id INTEGER NOT NULL REFERENCES users,
    created_at TIMESTAMP DEFAULT NOW()
);

ALTER TABLE phrases
	ADD FOREIGN KEY (chosen_alt)
	REFERENCES alternatives (id)
	DEFERRABLE INITIALLY DEFERRED;

CREATE TABLE "spellcheck"
(
    id SERIAL PRIMARY KEY,
    data JSONB,
    phrase_id INTEGER REFERENCES phrases,
    alt_id INTEGER REFERENCES alternatives,
    CONSTRAINT check_single_source CHECK (num_nonnulls(phrase_id,alt_id) = 1)
);
