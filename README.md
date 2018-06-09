## λ calculus interpreter (wip)

very simple lambda calculus interpreter to play with.   

![happy lambda](https://i.giphy.com/media/BSx6mzbW1ew7K/giphy.gif)


### working

tested on Ubuntu Xenial 16.04.3 LTS 
```bash
# deps
apt-get install cmake
apt-get install libreadline6 libreadline6-dev

# build
mkdir build
cd build && cmake ..
make
echo '(\x.x)(\x.x)awesome' | ./lc

```

Code is in a single header file (might change).

### TODO
* add more test
* implement scope object and assignment expressions
* implement numbers
* listen to clang-tidy...
* make main.cpp more unix-like


### references
* [De Bruijn Index](https://en.wikipedia.org/wiki/De_Bruijn_index)
* [To Mock a Mockingbird](https://en.wikipedia.org/wiki/To_Mock_a_Mockingbird)
* [Introduction to the Lambda Calculus](https://arxiv.org/abs/1503.09060)
