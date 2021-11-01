# May Haskell Backend

This is the [May](https://may.hazelfire.net/) backend. Written in Haskell.

It has three applications:
- Backend of May, written for AWS Lambda custom runtimes, integrates with AWS Cognito for Auth, Stripe for payments and DynamoDB for storage
- Backend Dev Server of May, written for development
- Standalone command line version of May

May is a task management application that gives you stats and insights on things like how
busy you are (urgency) and how much you can put off tasks (urgency) and recommends
the optimal task completion order.

The build system for this is in [Nix](https://nixos.org/manual/nix/stable/). It
interfaces with the [frontend](https://github.com/Hazelfire/may-elm/) using [GraphQL](https://github.com/Hazelfire/may-haskell/blob/master/schema.graphql).
