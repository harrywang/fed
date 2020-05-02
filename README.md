# About FED

Field Experiment Demystified (FED) via examples with code

## Setup

Setup Python and Jupyter:

- Python 3 required, see my tutorial to setup Python 3: https://bit.ly/2uX6wAX
- Clone the repo, go to the repo folder, setup the virtual environment, and install the required packages:

```shell
$ python3 -m venv venv
$ source venv/bin/activate
$ pip3 install -r requirements.txt
```

Setup R for Jupyter (Mac):

- install R: `brew install r`
- start R in terminal: `R`
- install IRkernel: first, in the R Console run: `install.packages('IRkernel')`, then install kernel spec for the current user: `IRkernel::installspec()`
- quit R `q()`

Run `$ jupyter notebook` to go over the notebooks.

<img width="315" alt="Screen Shot 2020-05-01 at 1 20 16 PM" src="https://user-images.githubusercontent.com/595772/80844283-52668700-8bd4-11ea-8b34-6e0c403a75b7.png">

## Disclaimer

The copyright (if any) of the data belongs to the original sources. This repo is only for learning and research.

For data from Marketing Science, we follow the Marketing Science replication policy and only use the downloaded file(s) for verifying replicability of the relevant paper's main results using the same data and model(s).
