module MailTest
import iTasks

spam :: Task [User]
spam = 
	enterInformation "Choose user" "Who do you want to spam?"
	>>= \user -> sendEmail "Test" (Note "Testerdetest") [user]

Start :: *World -> *World
Start world = startEngine [workflow "spam" "send test e-mail" spam] world