definition module Email
// This library sends emails from Clean using SMTP.

// Simple e-mail message, this type of message is the
// bare mimimum to send an e-mail.
:: Email		=	{ email_from	::	String
					, email_to		::	String
					, email_subject	::	String
					, email_body	::	String
					}
					
// For more advanced e-mail messages, there should be a
// MimeEmail type which can do cc,bcc,html-mail, attachments etc.

:: EmailOption	=	EmailOptSMTPServer	String

// Send function which sends the e-mail out
sendEmail :: [EmailOption] Email *World -> (Bool,*World)