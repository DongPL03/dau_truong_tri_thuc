package com.app.backend.services.verifyemail;

public interface IEmailVerificationService {
    void createAndSendTokenForUser(Long userId) throws Exception;
    void verifyToken(String token) throws Exception;
    void resendForEmail(String email) throws Exception;
}
