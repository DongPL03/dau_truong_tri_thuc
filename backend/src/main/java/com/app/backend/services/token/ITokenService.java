package com.app.backend.services.token;

import com.app.backend.models.NguoiDung;
import com.app.backend.models.Token;
import org.springframework.stereotype.Service;

@Service

public interface ITokenService {
    Token addToken(NguoiDung nguoiDung, String token, boolean isMobileDevice);

    Token refreshToken(String refreshToken, NguoiDung nguoiDung) throws Exception;

    void revokeAllTokensForUser(NguoiDung nguoiDung);

    void revokeToken(String token) throws Exception;

}
