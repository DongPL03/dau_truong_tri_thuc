package com.app.backend.services.verifyemail;

public interface IEmailService {
    void send(String from, String to, String subject, String htmlContent) throws Exception;
}
