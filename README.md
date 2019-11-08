
# About RuneSlayer

The general idea of RuneSlayer is to take the dopamine cycle/quick
gratification/addiction forming mechanisms that are often employed in computer
games (especially in the RPG genre) and use them for good, in the context of
natural languages learning. In a more concrete terms it will look like this:
your character will acquire "Runes" each representing a factoid in the data set
you are learning. You will answer question about these Runes and for correct
answers you will be rewarded with RP (Runic Points) and increase in the mastery
of that Rune. The Runic Points are needed to attack enemies and perform other
tasks (like maybe enchanting weapons, et c.) Once you master enough Runes you
will be able to learn more and also get some skill points to make your character
stronger.

The grand idea is to shorten the action-reward duration to increase motivation
when studying. When you are learning a natural language The Reward (being fluent
in that language) is very long time away and it's hard to use it to sustain the
motivation to preform the necessary tasks to achieve it, that is repeated study
sessions. Here you will have very short sessions (answering a handful of
questions) that you can control the duration of (how much RP you need) and that
you will need to do to almost immediately be granted a reward (progressing
further in the game, your character becoming stronger, et c.).

Obviously I don't believe that you could introduce this game to someone who just
wants to play a game and that he will accidentally learn a language. This is to
be rather a tool for people who already are in the mind to learn a language and
can use it as a crutch, to turn the pain of studying into pleasure of gaming
(and maybe even a small addiction, like many a game tend to be)

![Gameplay Screenshot][screenshot0]

The graphics is currently put together from open art I've found around the web.
Ideally I'd like to commission an artist to create art in a darker and more
realistic style, but it is what is is for now. (Coincidentally if you'd like to
hire/contract me, let me know, since I could use some currency now. I can
program in Haskell and do other "computer things", I can also juggle, so you
know, great value for your money).

Current version (0.1) is more of a demo, showcasing the vision of this a bit and
there isn't really more then 5 minutes of gameplay there (and even that is
a stretch). You start with 5 Runes (characters from hiragana a Japanese
syllabary, as an example for now) there is a short tutorial, you can kill some
enemies and get some items from chests distributed around the island, and that's
it.

There is quite a bit of core things to do still, some major things need to be
optimized, and so forth, but I feel like enough of the ground work is laid down
now to showcase this. So here we are.

# Setting things up

Currently I have only tested this on linux, and I still haven't setup CI to
produce binaries yet, so you'll going to have to build it yourself.

First you'll going to have to install nix if you don't have it already:

```
$ curl https://nixos.org/nix/install | sh
```

Then clone this repo & submodules:

```
$ git clone --recursive https://github.com/MaxOw/RuneSlayer
```

Start nix-shell (when running this command for the first time, even with binary
caches this will probably take a while so go make yourself some coffee / read
some news).

```
$ nix-shell
```

Then:

```
$ cabal configure; cabal build # also may take a while
$ dist/build/RuneSlayer/RuneSlayer # to run it
```

# Going forward

For this project to be successful in what it's embarking to do it will require
to have a great amount of content. Normally when a game has 80 hours of content
most people would say it's quite a lot. On the other hand, after spending 1000
hours learning a language, generally speaking, you will still be a beginner.
Everyone can see the discrepancy between these two numbers. That's why I would
like for this project to develop a community of contributors that would allow it
to grow organically to everyones benefit.

Therefor, in the future, I would like to semi-restrict access to this project to
contributors only. I say "semi-restrict" because since this repo is in public
and it will continue to stay in public anyone can just download and build it
themselves. It will only be a moral restriction. That's of course a talk for the
future, once there will be enough here that it's actually useful to anyone and
there is still a bit of a way to get there.

Let me also specify here what I mean by "contributor": often people tend to have
in excess of one of the two: time or money. If you have time, the more you can
contribute of course the better but even if you will just put 1-2 hours a month
fixing some typos or writing some flavor text, you will be morally recognized as
an official contributor in my eyes. Of course since that's just a moral
requirement you can ultimately judge that yourself, that was just a guideline.
For people with money I think some subscription model (like $20 a month or
something) would be nice. If you are an artist you can contribute art. If you
have money you can contribute that so we could hire an artist to make us some
art. Either way we all get more content.

Obviously, this project is a bit niche (I think) and there is quite high
probability that there wont be enough weirdos like me to form any sort of
community. In which case the whole point is moot, but this was just a "what if",
"ideal scenario" sort of thing.

Also there is a possibility that I will get bored/tired with this project at
some point and then I will likely release it under some permissive copyright (or
copyleft) license, but for now it is what it is for the reasons stated above.

# License
Copyright (c) 2018-2019, Maksymilian Owsianny

With exception of points henceforth specified, all rights reserved.

You are free to build this code and use the generated binary, or use the
binaries provided here for your own personal use.

You are free to fork this code as a means of contributing back to this project,
however you are not free to fork it as a means of splitting the community, that
is if you disagree with the choices I make regarding this project and want to
take it in some other direction.

Anything you contribute to this project you grant me all rights, with exception
of the right of being called the author of that contribution. Afterwords you
will not be able to restrict any of my rights to that contribution, and
similarly I will not be able to restrict yours as if we were to coincidentally
create exactly the same work independently.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

[screenshot0]: https://raw.githubusercontent.com/MaxOw/RuneSlayer-media0/master/screenshoot0_cropped.png
