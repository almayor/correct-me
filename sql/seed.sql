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
BEGIN

INSERT INTO phrases (text, author_id)
VALUES ('phrase1 by testuser1', (SELECT id FROM users WHERE username = 'testuser1'))
RETURNING id INTO new_phrase_id;

INSERT INTO alternatives (phrase_id, text, author_id)
VALUES (new_phrase_id, 'alt to phrase1 by testuser1', (SELECT id FROM users WHERE username = 'testuser1'));

INSERT INTO alternatives (phrase_id, text, author_id)
VALUES (new_phrase_id, 'alt to phrase1 by testuser2', (SELECT id FROM users WHERE username = 'testuser2'));

END$$;

-- inserting phrase2 and its alternatives
-- choosing alternative to phrase2
DO $$
DECLARE new_phrase_id PHRASES.id%TYPE;
DECLARE new_alt_id ALTERNATIVES.id%TYPE;
BEGIN

INSERT INTO phrases (text, author_id)
VALUES ('phrase2 by testuser3', (SELECT id FROM users WHERE username = 'testuser3'))
RETURNING id INTO new_phrase_id;

INSERT INTO alternatives (phrase_id, text, author_id)
VALUES (new_phrase_id, 'first alt to phrase2 by testuser1', (SELECT id FROM users WHERE username = 'testuser1'));

INSERT INTO alternatives (phrase_id, text, author_id)
VALUES (new_phrase_id, 'second alt to phrase2 by testuser1', (SELECT id FROM users WHERE username = 'testuser1'));

INSERT INTO alternatives (phrase_id, text, author_id)
VALUES (new_phrase_id, 'alt to phrase2 by testuser2', (SELECT id FROM users WHERE username = 'testuser2'))
RETURNING id INTO new_alt_id;

UPDATE phrases
SET chosen_alt_id = new_alt_id, is_open = FALSE
WHERE id = new_phrase_id;

END $$;
