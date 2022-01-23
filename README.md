# ![RealWorld Example App](logo.png)

![GitHub commit activity](https://img.shields.io/github/commit-activity/m/jchavarri/jsoo-react-realworld-example-app)
![GitHub last commit](https://img.shields.io/github/last-commit/jchavarri/jsoo-react-realworld-example-app)
![GitHub](https://img.shields.io/github/license/jchavarri/jsoo-react-realworld-example-app)

> ### Js_of_ocaml + React codebase containing real world examples (CRUD, auth, advanced patterns, etc) that adheres to the [RealWorld](https://github.com/gothinkster/realworld) spec and API.

### [Demo](https://jchavarri.github.io/jsoo-react-realworld-example-app/)&nbsp;&nbsp;&nbsp;&nbsp;[RealWorld](https://github.com/gothinkster/realworld)

This codebase was created to demonstrate a fully fledged fullstack application built with **[Js_of_ocaml](http://ocsigen.org/js_of_ocaml/latest/manual/overview)** and **[jsoo-react](https://github.com/ml-in-barcelona/jsoo-react)** including CRUD operations, authentication, routing, pagination, and more.

For more information on how to this works with other frontends/backends, head over to the [RealWorld](https://github.com/gothinkster/realworld) repo.

# How it works

Basically its just like React single-page-application but written in [OCaml](https://ocaml.org/) with [React](https://reactjs.org/).

- Using [Webpack](https://webpack.js.org/) as the frontend build tool
- Using [Opam](https://opam.ocaml.org/) as OCaml package manager, [Yarn](https://yarnpkg.com/) as JavaScript package manager
- Using [Dune](https://dune.readthedocs.io/en/stable/) as OCaml code build tool
- Routing - jsoo-react [Router](https://github.com/ml-in-barcelona/jsoo-react/blob/main/lib/router.mli)

# Getting started

You will need to install both [Opam](https://opam.ocaml.org/doc/Install.html) and [Yarn](https://yarnpkg.com/getting-started/install) package managers.

To get the project running locally:

```bash
git clone https://github.com/jchavarri/jsoo-react-realworld-example-app.git
cd jsoo-react-realworld-example-app
yarn install # install JavaScript dependencies
make create-switch # create Opam local switch
make init # install OCaml dependencies
make build # build OCaml sources, generate JavaScript
make start # bundle with Webpack, start dev web server
```

Then open http://localhost:8000 to see your app.

# Acknowledgements

This projects was adapted from https://github.com/jihchi/rescript-react-realworld-example-app by [@jihchi](https://github.com/jihchi).
