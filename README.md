# gedcom-importer

A GEDCOM to Geni API importer tool.

The idea is to take a GEDCOM file and import the family records into
Geni via its API.

## Usage

This importer is meant for bigger and better things in the future, but for now
you can try it out yourself. Keep in mind that this is not meant to be a
user-facing thing. It isn't meant to be easy to use for people other than
Clojure programmers. Anyways, here are instructions for using it to import a
GEDCOM file into the production geni website.

### Step 1 - Download Leiningen

The first thing you'll want to do is download
[Leiningen](https://github.com/technomancy/leiningen). This program is the user
interface/build tool for Clojure. This is equivalent to 'installing'
Clojure. Follow the instructions there.

### Step 2 - Get an API key from Geni

If you want to import gedcoms, chances are you have a Geni account. If not,
you'll need one. Go get one.

Next, go to
[the Geni API explorer](https://www.geni.com/platform/developer/api_explorer). You
can get a magic API key here. Just click "Get Access Token" and it should give
you a fancy new key. This API key can be used with your account to upload a
gedcom to it.

### Step 3 - Get this importer!

You need git to fetch the importer. If you're on OS X, `brew install git`, or
whatever equivalent for your Linux package manager. Once done:

```
git clone git@github.com:geni/geni-gedcom
cd geni-gedcom
```

Almost there!

### Step 4 - Use it

Now you have two options. You can either use the little web interface that we
use for testing the importer, or you can just call the code directly. I'll show
both options. First of all, let's try the web interface.

Open `resources/gedcom.properties` in your favorite text editor and change it to
look like this:

```
url=https://www.geni.com/api
base.url=
insecure=false
```

Next, run `lein ring server`. This might take a while. It'll download a **lot** of
stuff. Just give it a bit. Once it is finished, it should automatically open the
interface in your browser. 

* Enter your token into the text field
* Click the choose button and select the GEDCOM you want to import.
* Wait for the above to finish to finish parsing.
* Select a person to start with in the dropdown list that has just magically
  appeared.
* Click 'choose'.
* Wait for it to upload, or go to `localhost:3000/progress?token=yourtoken` and
  watch refresh constantly with excitement to see how close you are to being finished.


## License

Distributed under the Eclipse Public License, the same as Clojure.
