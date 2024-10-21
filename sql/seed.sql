-- seed PSQL file used for testing purposes
-- inserts user with the name 'testuser1' and hash of the password 'testpassword1'
INSERT INTO users (username, password)
VALUES ('testuser1', '$2a$12$ALZtguUxfgkOM92gBcC97ej1YQTey43yCTMvBKLDSk1nDN5mcM/OO');

-- inserts user with the name 'testuser2' and hash of the password 'testpassword2'
INSERT INTO users (username, password)
VALUES ('testuser2', '$2a$12$IKVel.XSCJoLWId6nJKF7.ezwUc726ESzPFgRA.sPu8cQsQGk44V2');

-- inserts user with the name 'testuser3' and hash of the password 'testpassword3'
INSERT INTO users (username, password)
VALUES ('testuser3', '$2a$12$3t1JnhMJ.HLddNlyn.8Keekk00LdZVaYwpiRE8vckUjq7LUm3.vXC');

-- inserting phrase1 and its alternatives
DO $$
DECLARE new_phrase_id PHRASES.id%TYPE;
DECLARE new_phrase_spellcheck_id SPELLCHECK.id%TYPE;
DECLARE new_altA_spellcheck_id SPELLCHECK.id%TYPE;
DECLARE new_altB_spellcheck_id SPELLCHECK.id%TYPE;
BEGIN

INSERT INTO spellcheck (data)
VALUES ('[]')
RETURNING id INTO new_phrase_spellcheck_id;

INSERT INTO phrases (text, author_id, spellcheck_id)
VALUES ('phrase1 by testuser1', (SELECT id FROM users WHERE username = 'testuser1'), new_phrase_spellcheck_id)
RETURNING id INTO new_phrase_id;

INSERT INTO spellcheck (data)
VALUES ('[]')
RETURNING id INTO new_altA_spellcheck_id;

INSERT INTO alternatives (phrase_id, text, author_id, spellcheck_id)
VALUES (new_phrase_id, 'alt to phrase1 by testuser1', (SELECT id FROM users WHERE username = 'testuser1'), new_altA_spellcheck_id);

INSERT INTO spellcheck (data)
VALUES ('[]')
RETURNING id INTO new_altB_spellcheck_id;

INSERT INTO alternatives (phrase_id, text, author_id, spellcheck_id)
VALUES (new_phrase_id, 'alt to phrase1 by testuser2', (SELECT id FROM users WHERE username = 'testuser2'), new_altB_spellcheck_id);

END$$;

-- inserting phrase2 and its alternatives
-- choosing alternative to phrase2
DO $$
DECLARE new_phrase_id PHRASES.id%TYPE;
DECLARE new_alt_id ALTERNATIVES.id%TYPE;
DECLARE new_phrase_spellcheck_id SPELLCHECK.id%TYPE;
DECLARE new_altA_spellcheck_id SPELLCHECK.id%TYPE;
DECLARE new_altB_spellcheck_id SPELLCHECK.id%TYPE;
DECLARE new_altC_spellcheck_id SPELLCHECK.id%TYPE;
BEGIN

INSERT INTO spellcheck (data)
VALUES ('[{"code":1,"pos":0,"row":0,"col":0,"len":7,"word":"phrase2","s":["phrase 2","phase 2","phrases 2"]}]')
RETURNING id INTO new_phrase_spellcheck_id;

INSERT INTO phrases (text, author_id, spellcheck_id)
VALUES ('phrase2 by testuser3', (SELECT id FROM users WHERE username = 'testuser3'), new_phrase_spellcheck_id)
RETURNING id INTO new_phrase_id;

INSERT INTO spellcheck (data)
VALUES ('[{"code":1,"pos":13,"row":0,"col":13,"len":7,"word":"phrase2","s":["phrase 2","phrase2"]}]')
RETURNING id INTO new_altA_spellcheck_id;

INSERT INTO alternatives (phrase_id, text, author_id, spellcheck_id)
VALUES (new_phrase_id, 'first alt to phrase2 by testuser1', (SELECT id FROM users WHERE username = 'testuser1'), new_altA_spellcheck_id);

INSERT INTO spellcheck (data)
VALUES ('[{"code":1,"pos":14,"row":0,"col":14,"len":7,"word":"phrase2","s":["phrase 2","phrase2","phase 2"]}]')
RETURNING id INTO new_altB_spellcheck_id;

INSERT INTO alternatives (phrase_id, text, author_id, spellcheck_id)
VALUES (new_phrase_id, 'second alt to phrase2 by testuser1', (SELECT id FROM users WHERE username = 'testuser1'), new_altB_spellcheck_id);

INSERT INTO spellcheck (data)
VALUES ('[]')
RETURNING id INTO new_altC_spellcheck_id;

INSERT INTO alternatives (phrase_id, text, author_id, spellcheck_id)
VALUES (new_phrase_id, 'alt to phrase2 by testuser2', (SELECT id FROM users WHERE username = 'testuser2'), new_altC_spellcheck_id)
RETURNING id INTO new_alt_id;

UPDATE phrases
SET chosen_alt_id = new_alt_id, is_open = FALSE
WHERE id = new_phrase_id;

END $$;
