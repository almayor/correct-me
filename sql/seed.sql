-- seed PSQL file used for testing purposes
-- inserts user with the name 'testuser1' and hash of the password 'testpassword1'
INSERT INTO users (username, password)
VALUES ('testuser1', '$2a$12$ALZtguUxfgkOM92gBcC97ej1YQTey43yCTMvBKLDSk1nDN5mcM/OO');

-- inserts user with the name 'testuser2' and hash of the password 'testpassword2'
INSERT INTO users (username, password)
VALUES ('testuser2', '$2a$12$IKVel.XSCJoLWId6nJKF7.ezwUc726ESzPFgRA.sPu8cQsQGk44V2');

-- user with the name 'testuser1' inserts a phrase storing its id into a variable
-- user with the name 'testuser2' then inserts an alternative to the phrase
DO $$
DECLARE new_phrase_id PHRASES.id%TYPE;
BEGIN

INSERT INTO phrases (text, author_id)
VALUES ('This is a phrase', (SELECT id FROM users WHERE username = 'testuser1'))
RETURNING id INTO new_phrase_id;

INSERT INTO alternatives (phrase_id, text, author_id)
VALUES (new_phrase_id, 'This is testuser1''s phrase', (SELECT id FROM users WHERE username = 'testuser2'));

END $$