-- seed PSQL file used for testing purposes
-- inserts user with the name 'testuser1' and hash of the password 'testpassword1'
INSERT INTO users (id, username, password)
VALUES ('testuser1', '$2a$12$ALZtguUxfgkOM92gBcC97ej1YQTey43yCTMvBKLDSk1nDN5mcM/OO');

-- inserts user with the name 'testuser2' and hash of the password 'testpassword2'
INSERT INTO users (username, password)
VALUES ('testuser2', '$2a$12$IKVel.XSCJoLWId6nJKF7.ezwUc726ESzPFgRA.sPu8cQsQGk44V2');

-- user with the name 'testuser1' inserts a phrase
INSERT INTO phrases (id, text, user_id)
VALUES ('This is a phrase', (SELECT id FROM users WHERE username = "testuser1"));

-- user with the name 'testuser2' inserts an alternative to the phrase
INSERT INTO alternatives (phrase_id, text, user_id)
VALUES ('This is testuser1''s phrase', (SELECT id FROM users WHERE username = "testuser2"));
