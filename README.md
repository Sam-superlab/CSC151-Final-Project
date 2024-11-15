# CSC151-Final-Project

Authors: Xuyi(Sam) Ren, Hassan, Jonathan Gaynor, Ari Johnson, Sam

## Project Description

## How to contribute(use git)

git is a version control system that allows multiple people to work on the same project. This is a quick guide on how to use git to contribute to this project.

Here are some commands that you can make changes and contribute to the project in your local machine:

```
git clone https://github.com/Sam-superlab/CSC151-Final-Project
```

after running this command, you will have a copy of the project on your local machine. You can now make changes to the project. you can use github desktop to make changes to the project. It is a GUI tool that makes it easier to make changes to the project. Note that you should always comment your work before you push it to the github repository.

in terminal, you can use the following commands to push your changes to the github repository:

```
git commit -m "your comment here"
```

note that you should create a branch for your work. You can create a branch using the following command:

```
git checkout -b your-branch-name
```

It will create a new branch and switch to it. You can now make changes to the project. Once you are done making changes, you can push your changes to the github repository using the following command:

```
git push origin your-branch-name
```

However, if it fails, probably because you are not up-to-date with the main branch, you can use the following command to pull the changes from the main branch to your branch:

```
git pull origin main
```

If you have any conflicts, you should resolve them before you can push your changes to the github repository. Once you have resolved the conflicts, you can push your changes to the github repository using the following command:

```
git push origin your-branch-name
```

After you checked that your changes are correct, you can create a pull request to merge your changes to the main branch. You can create a pull request by going to the github repository and clicking on the "New pull request" button. You can then select the branch that you want to merge and create a pull request. Once you have created a pull request, you can assign someone to review your changes and merge them to the main branch. Here is the code for the pull request:

```
git pull-request
```

This is a quick guide on how to use git to contribute to this project. If you have any questions, feel free to ask.
