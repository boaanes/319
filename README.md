To run this application, either clone it and use docker compose, or spin up each container on their own.

**Without using docker compose:**

For the server:
```sh
docker run --name haskell-server -p 8000:8000 -d boaanes/319-haskell-server
```

For the frontend:
```sh
docker run --name react-app --link haskell-server:haskell-server -p 80:80 -d boaanes/319-react-app
```

Then go to [localhost](http://localhost) in your browser.

**Using docker compose:**

Clone this repository:

```sh
git clone git@github.com:boaanes/319.git
```

Navigate to the folder:

```sh
cd 319
```

Run docker compose:

```sh
docker compose up
```

Then go to [localhost](http://localhost) in your browser.
