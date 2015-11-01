# git-move

## Moving to a new position

When you decide that the current branch needs to use a different hash
and content, currently you need to `reset --hard` to the new position.

However, `reset --hard` has the annoying side-effect of deleting any
outstanding work.

It would be nice to be able to move the tip of the current branch to a
new position and update the working tree, as long as there's no
overlap between files changed in the working tree and in the diff to
the remote location.

Just type:

```
git-move <refspec>
```

To move to the new location.

## Updating a local tracking branch with no local commits

When you have a local branch that is tracking a remote branch (and has
no local commits) that was rebased, you would have to `reset --hard`
to the remote location. This requires typing out the name of the
upstream branch (or "@{u}" at least), and as usual, loses any
uncommitted changes.

Just type:

```
git-move
```

With no parameters, and it will move to the upstream location, taking
any uncommitted changes on top.
