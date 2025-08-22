# Dmarc

To start your Phoenix server:

  * Run `mix setup` to install and setup dependencies
  * Start Phoenix endpoint with `mix phx.server` or inside IEx with `iex -S mix phx.server`

Now you can visit [`localhost:4000`](http://localhost:4000) from your browser.

Ready to run in production? Please [check our deployment guides](https://hexdocs.pm/phoenix/deployment.html).

## Project Setup TODOs
- generate auth models
- initialize database
- integrate daisy.ui ((p)npm)
- generate docker release files
- adjust Dockerfile
- generate release files

## Local Development

- run authelia, `../authelia/bin/start-authelia.sh`
- run caddy, `../caddy/bin/start-caddy.sh`
- run existdb, `../existdb/bin/start-existdb`
- run webapp, `EXISTDB_URL=http://localhost:8080/exist/rest/dmarc EXISTDB_USER=admin EXISTDB_PASSWORD="" PORT=8081 MIX_ENV="dev" iex -S mix phx.server`

## Learn more

  * Official website: https://www.phoenixframework.org/
  * Guides: https://hexdocs.pm/phoenix/overview.html
  * Docs: https://hexdocs.pm/phoenix
  * Forum: https://elixirforum.com/c/phoenix-forum
  * Source: https://github.com/phoenixframework/phoenix

# Authentication

Yes, absolutely! JWTs (JSON Web Tokens) can indeed be used in a server-rendered web application to avoid the traditional
server-side session store. This is a common pattern often referred to as "stateless sessions" or "JWT-based sessions."

The core idea is that all the necessary session data (user ID, roles, expiry, etc.) is encoded directly into the JWT,
which is then stored in a cookie on the client-side. When a request comes in, the server decodes and verifies the JWT
in the cookie, extracting the user's authenticated identity and other claims without needing to consult a central
session database.

Here's how it works and its implications:

How JWT-based Sessions Work in Server-Rendered Apps:

Authorization Code Flow Completion (Backend):
- User authenticates with IdP (e.g., Google).
- IdP redirects back to your backend's redirect URI with an authorization_code.
- Your backend exchanges this authorization_code with the IdP for access_token, refresh_token, and id_token.
- Your backend validates the id_token (signature, issuer, audience, expiry) to confirm user identity.
- Your backend now has the user's verified identity and a refresh_token for long-lived sessions.

Application JWT Issuance (Backend to Frontend):
- Upon successful id_token validation and user identification, your backend generates an application-specific JWT.
- Contents of the JWT:
  - Standard JWT claims: sub (user ID), iss (your backend), aud (your app), exp (expiration), iat (issued at).
  - Any user-specific data or authorization claims that your application needs for the request (e.g., roles,
    permissions, preferences). This is the key difference from a traditional session ID – the data is in the
    token, not pointed to by an ID.
- Token Storage (Backend to Frontend): Your backend sends this JWT to the frontend as an HTTP-only, Secure,
  SameSite=Lax (or Strict) Cookie.
  - HTTP-only: Prevents JavaScript access (critical for security).
  - Secure: Only sent over HTTPS.
  - SameSite: Mitigates CSRF (though a separate CSRF token is still highly recommended for server-rendered forms).

Frontend Requests (Client to Backend):
- For every subsequent request, the browser automatically sends the HTTP-only cookie containing the JWT.
- Verification on Each Backend Request (Backend):
- Your backend receives the request with the JWT in the cookie.
- It parses the JWT from the cookie.
- It verifies the JWT's signature using your backend's secret/public key.
- It validates the JWT's claims:
  - exp (Expiration): Check if the token has expired.
  - iss (Issuer), aud (Audience): Ensure it's for your application.
  - Any other required claims.
- If valid, the backend trusts the claims in the JWT (e.g., sub, roles, etc.) and uses them to identify and
  authorize the user for the current request.
- If invalid (e.g., expired, tampered, bad signature), the backend returns an authentication error
  (401 Unauthorized) or, more commonly in server-rendered apps, performs an HTTP redirect to the login page.

Handling Expiration (with a Stateless Backend):
This is the trickiest part of stateless sessions in a server-rendered context, as you don't have a backend
session store to manage refresh tokens directly.

Short-Lived Application JWTs:
- The JWT in the cookie should still have a relatively short expiration time (e.g., 15-30 minutes, or
  even a few hours, depending on risk tolerance and user tolerance for re-login). This is your primary "session" timeout.

Long-Lived IdP Refresh Tokens (Backend Persistence):
- You still need to store the refresh_token from the IdP securely on your backend. This is the only piece of
  "state" you'll typically maintain for long-term user login. It's stored in a database associated with your internal
  user ID. This is not a "session store" in the traditional sense, but a user_id <-> refresh_token mapping.
- This refresh_token allows you to obtain new IdP access_tokens and new application JWTs for your user without making
  them re-login with the IdP every time their short-lived JWT expires.

The "Silent Refresh" Challenge (and Solution):
Problem: In a server-rendered app, the browser doesn't have JavaScript to silently detect an expired JWT and
proactively request a new one before an API call. Every request involves a full page load or form submission.

Solution (Backend-Driven Refresh):
When your backend receives a request with an expired application JWT:
- Instead of immediately redirecting to login, it checks if it has a valid stored refresh_token for the user
  (identified by the sub claim in the expired JWT, or perhaps a separate, encrypted user ID in another cookie).

If a valid refresh_token exists:
- Your backend uses this refresh_token to call the IdP's token endpoint to get a new access_token (and potentially
  a new refresh_token from the IdP).
- Upon success, your backend generates a new application JWT with a fresh expiry.
- It sets this new JWT as an HTTP-only, Secure cookie in the response.
- It then proceeds to process the original request using the claims from the newly issued JWT, effectively making
  the refresh process transparent to the user.

Rotate Refresh Tokens: As before, update the stored refresh token if a new one is issued by the IdP.
If no valid refresh_token is found (e.g., expired, revoked, or none stored):
- The backend clears the old JWT cookie.
- It issues an HTTP redirect to the login page, forcing a full re-authentication with the IdP.

Diagram of JWT Session Expiration Handling (Server-Rendered):

[Browser]
    |
1. User Request (with Expired JWT in Cookie)
    |----------------------------> [Backend]
    |                                 |
    |                                 2. Verify JWT: Signature OK, but `exp` expired.
    |                                 3. Backend checks if it has a valid IdP Refresh Token for the user (from DB)
    |
    |                                 IF IdP Refresh Token is Valid:
    |                                     4a. Use IdP Refresh Token to get NEW IdP Access/Refresh Tokens
    |                                     5a. Update stored IdP Refresh Token in DB (if rotated)
    |                                     6a. Generate NEW Application JWT with fresh expiry
    |                                     7a. Set-Cookie: new_jwt=... (HTTP-only, Secure)
    |                                     8a. Process Original Request, Render View
    |<--------------------------- 9a. HTML Response (with new JWT cookie set)
    |
    |                                 IF IdP Refresh Token is NOT Valid / Not Found:
    |                                     4b. Clear old JWT cookie
    |                                     5b. HTTP 302 Redirect to /login (initiates new IdP flow)
    |<--------------------------- 6b. HTTP 302 Redirect to /login

Advantages of JWTs for Server-Rendered Apps (Stateless Sessions):
- Scalability: No central session store means easier horizontal scaling of your backend servers. Each server
  can independently verify JWTs using a shared secret/public key.
- Reduced Database Load: Fewer database lookups per request compared to traditional session stores.
- Simpler Backend Code (in some aspects): No need for session cleanup, session persistence logic (beyond
  the refresh token).
- Microservices Friendly: Easy to share authentication state across different microservices if they all share
  the same JWT signing key and validation logic.

Disadvantages/Considerations:
- JWT Size: If you put too much data in the JWT, the cookie size can increase, potentially impacting request
  headers. Keep claims lean.
- Immutability: Once a JWT is issued, it's valid until it expires. Immediate revocation (e.g., user logs out,
  password change, account disabled) is harder.

Workarounds for Revocation:
- Short Expiration: Rely on the short expiry.
- Blacklist/Blocklist: Maintain a small, fast-lookup blacklist (e.g., Redis cache) of revoked JWT IDs
  (jti claim). This reintroduces some state but for revocation only.
- Refresh Token Rotation: If the backend rotates refresh tokens, a stolen access token will eventually expire,
  and the old refresh token can be denied on next use.

Security of Refresh Token: The refresh_token from the IdP is still a critical piece of state that must be
stored securely on your backend (encrypted at rest, never exposed).

CSRF Protection: While HTTP-only and SameSite cookies help, traditional CSRF tokens in forms are still
crucial for server-rendered applications to prevent cross-site request forgery. This is separate from JWT
handling but essential for overall security.

Using JWTs in server-rendered applications is a valid and often preferred approach for building scalable
and decoupled authentication systems, but it requires careful consideration of the expiration and revocation
strategies.


