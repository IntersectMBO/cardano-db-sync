to build against mainnet:
`time docker build --build-arg environment=mainnet  -t cardano-explorer-mainnet .`

when the build is done, the last few lines of output should look similar to:
```
Step 24/25 : EXPOSE 80
 ---> Running in f1c87c519146
Removing intermediate container f1c87c519146
 ---> 513fcf74049f
Step 25/25 : ENTRYPOINT [ "/nix/var/nix/profiles/per-user/cardano/profile/bin/runit" ]
 ---> Running in bfb064b25056
Removing intermediate container bfb064b25056
 ---> 20605fdffcdc
Successfully built 20605fdffcdc
Successfully tagged cardano-explorer-mainnet:latest

real    34m26.889s
user    0m2.095s
sys     0m3.377s
```

you can then start the container with: `docker run --rm -t -i -p 80:80 --name test-image --volume explorer-mainnet:/var cardano-explorer-mainnet:latest`

http://localhost/grafana/login and login as admin/admin to view the performance metrics and sync state

example of using the explorer api:
```
$ curl http://localhost/api/supply/ada
{"Right":31112479913.565959}
```

http://localhost/api/submit/tx is the tx submission endpoint


to build an image that can target multiple clusters:
`time docker build --build-arg environment=all  -t cardano-explorer-all .`

then to select a cluster, use one of the following:
```
docker run --rm -i -t -p 81:80 --name explorer-mainnet --volume explorer-mainnet:/var -e ENVIRONMENT=mainnet cardano-explorer-all:latest
docker run --rm -i -t -p 81:80 --name explorer-testnet --volume explorer-testnet:/var -e ENVIRONMENT=testnet cardano-explorer-all:latest
```
