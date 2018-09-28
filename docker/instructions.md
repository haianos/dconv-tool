# Instructions for the docker container

This is a brief overview of the steps to build and use the docker container 
with `dconv-tool`.

## Build the container
```
docker build  -t haianos/dconv-release:xenial  .
```

## Run the container
```
docker run -it haianos/dconv-release:xenial
```


## Testing the software

The container has a clone of the `dconv-tool` repository.
All the provided tests must succeed, and they can be found in
`busted` subfolder.
In order to run the tests, execute (from `/home/user/dconv-tool` folder):

```
lua busted/testX/testX.lua
```
where `X` is the number of the test you want to try (`1` to `4`).

Moreover, the docker image as the simple `ROS2/rclc` testcase for the runtime of the dconv-tool (dynamic conversion).
The sources of this test can be found at [https://github.com/haianos/dconv-rclc-example](https://github.com/haianos/dconv-rclc-example).

To try this out, jump to the subfolder `/home/user/rosws/src/dconv-rclc-example/dconv_rclc_example/script` and execute:
```
subme datamode.dproto init-dconv.lua &
publishme &
```
The expected output is two-line printouts each second, that looks like:
```
Publishing: [x: 1.100000,y: 2.000000,z: 6.500000]
USER: 1.100000 2.000000 6.500000
Publishing: [x: 1.200000,y: 2.000000,z: 6.400000]
USER: 1.200000 2.000000 6.400000
Publishing: [x: 1.300000,y: 2.000000,z: 6.300000]
USER: 1.300000 2.000000 6.300000
Publishing: [x: 1.400000,y: 2.000000,z: 6.200000]
USER: 1.400000 2.000000 6.200000
Publishing: [x: 1.500000,y: 2.000000,z: 6.100000]
USER: 1.500000 2.000000 6.100000
Publishing: [x: 1.600000,y: 2.000000,z: 6.000000]
USER: 1.600000 2.000000 6.000000
Publishing: [x: 1.700000,y: 2.000000,z: 5.900000]
USER: 1.700000 2.000000 5.900000
```
The first values (Publishing) are the values sent by a ROS producer node, of type `geometry_msgs/Point`.
The values in `USER` are printouts of the internal datatype used as digital data representation (`double[3]`)
in the consumer node, after the conversion.
The `dproto` models can be found in `datamodel.dproto` file.
