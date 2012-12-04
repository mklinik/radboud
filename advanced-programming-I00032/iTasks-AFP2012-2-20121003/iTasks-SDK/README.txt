This repository holds the iTasks Software Development Kit (SDK).
=== Setup ===

= Preparation of the IDE =
- Make sure that this SDK is placed in the folder of the Clean 2.4 32-bit distribution (the one that contains "CleanIDE.exe")
  and is called "iTasks-SDK".
- Copy the files "iTasks-SDK/Compiler/StdGeneric.dcl" and "iTasks-SDK/Compiler/StdGeneric.icl" to "Libraries/StdEnv/" (overwriting the existing files)
- Copy the files "iTasks-SDK/Compiler/TCPChannels.dcl" and "iTasks-SDK/Compiler/TCPChannels.icl" to "Libraries/TCPIP/" (overwriting the existing files)
- Copy the files "iTasks-SDK/Compiler/_SystemDynamic.dcl" and "iTasks-SDK/Compiler/_SystemDynamic.icl" to "Libraries/Dynamics/implementation" (overwriting the existing files)
- Start the CleanIDE
- Import the "iTasks" environment by choosing "Environment" -> "Import..." from the menu
  and selecting the "iTasks-SDK/Server/iTasks.env" file. 
- Set your default heap size for projects to 8M by choosing "Project" -> "Project defaults..." from the menu

= Additional steps if you need client side execution =
- Please note that it works only with the Clean 2.4 32-bit distribution
- Import the "iTasks + SAPL" environment by choosing "Environment" -> "Import..." from the menu
  and selecting the "iTasks-SDK/Server/iTasks + Sapl.env" file. 
- Unpack "iTasks-SDK/Compiler/StdEnv-Sapl.zip" into "Libraries/StdEnv/"
- When you want to compile the current project to SAPL, choose "iTasks+SAPL" environment from the menu,
  and press CTRL-SHIFT-U to force the recompilation of the whole project.
  The compilation process creates a directory called "sapl" which contains all the necessary SAPL files;
  this directory will be used by the client side execution infrastucture of the iTask toolkit.
  
= Building the support tools ==
- Build the RunAsync tool by opening the Clean project "iTasks-SDK/Tools/RunAsync/RunAsync.prj" and choosing ("Project" -> "Update") from the menu.

= Building examples =
The most up-to-date example suite to run at the moment is the examples collection for the CEFP Summerschool.

- Open the "iTasks-SDK/Examples/BasicAPIExamples.prj" Clean project. ("File" -> "Open...")
- You build the project by choosing ("Project" -> "Update and Run") from the menu.
- A BasicAPIExamples.exe server is started automatically which you can access at "http://localhost/"
- Further instructions for setting up are given by the server 

=== Content of the repository ===

This SDK consists of multiple libraries, tools and documents that are needed to
create iTasks applications.
They are divided over multiple folders as follows:

- Server   : Here you find all the Clean libraries that are needed to build iTasks
             server applications. As a workflow programmer you will need these libraries
             in combination with the Clean IDE and build system to program workflow management
             applications.
             The "Server" folder is divided in an "API" and "Framework" folder. In API you find
             the modules that make up the iTasks workflow definition language. You can use these
             to specify your workflows with combinators. The "Framework" folder contains all the
             magic that is needed to turn workflow specifications into executable systems. As a
             workflow programmer you will not need to get to know these libraries.

- Client   : To let end-users interact with the workflow management systems you create with
             the server libraries, they need a client application. In this folder you find the
             default AJAX web client which your server application will serve at run time.

- Examples : Example task definitions.

- Tools    : Various build and code generation tools.
