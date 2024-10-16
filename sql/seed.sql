-- seed PSQL file used for testing purposes
-- inserts manager with the name 'testuser1' and hash of the password 'testpassword1'
INSERT INTO users (id, username, password)
VALUES (1, 'testuser1', '$2a$12$ALZtguUxfgkOM92gBcC97ej1YQTey43yCTMvBKLDSk1nDN5mcM/OO');

-- inserts manager with the name 'testuser2' and hash of the password 'testpassword2'
INSERT INTO users (id, username, password)
VALUES (2, 'testuser2', '$2a$12$IKVel.XSCJoLWId6nJKF7.ezwUc726ESzPFgRA.sPu8cQsQGk44V2');

INSERT INTO phrases (id, text, user_id)
VALUES (1, 'This is a phrase', 1);

INSERT INTO alternatives (phrase_id, text, user_id)
VALUES (1, 'This is testuser1''s phrase', 2);
