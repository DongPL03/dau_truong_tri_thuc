package com.app.backend.services.token;


import com.app.backend.components.JwtTokenUtils;
import com.app.backend.exceptions.DataNotFoundException;
import com.app.backend.exceptions.ExpiredTokenException;
import com.app.backend.models.NguoiDung;
import com.app.backend.models.Token;
import com.app.backend.repositories.ITokenRepository;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.time.Instant;
import java.time.ZoneId;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class TokenService implements ITokenService {
    private static final int MAX_TOKENS = 3;
    @Value("${jwt.expiration}")
    private int expiration; //save to an environment variable

    @Value("${jwt.expiration-refresh-token}")
    private int expirationRefreshToken;

    private final ITokenRepository tokenRepository;
    private final JwtTokenUtils jwtTokenUtil;

    @Transactional
    @Override
    public Token refreshToken(String refreshToken, NguoiDung user) throws Exception {
        Token existingToken = tokenRepository.findByRefreshToken(refreshToken);
        if (existingToken == null) {
            throw new DataNotFoundException("Refresh token does not exist");
        }
        if (existingToken.getRefreshExpirationDate().compareTo(Instant.now()) < 0) {
            tokenRepository.delete(existingToken);
            throw new ExpiredTokenException("Refresh token is expired");
        }
        String token = jwtTokenUtil.generateToken(user);
        Instant expirationDateTime = Instant.now().plusSeconds(expiration);
        existingToken.setExpirationDate(expirationDateTime);
        existingToken.setToken(token);
        existingToken.setRefreshToken(UUID.randomUUID().toString());
        existingToken.setRefreshExpirationDate(Instant.now().plusSeconds(expirationRefreshToken));
        return existingToken;
    }

    @Transactional
    @Override
    public Token addToken(NguoiDung user, String token, boolean isMobileDevice) {
        List<Token> userTokens = tokenRepository.findByNguoiDung(user);
        int tokenCount = userTokens.size();
        // Số lượng token vượt quá giới hạn, xóa một token cũ
        if (tokenCount >= MAX_TOKENS) {
            //kiểm tra xem trong danh sách userTokens có tồn tại ít nhất
            //một token không phải là thiết bị di động (non-mobile)
            boolean hasNonMobileToken = !userTokens.stream().allMatch(Token::isMobile);
            Token tokenToDelete;
            if (hasNonMobileToken) {
                tokenToDelete = userTokens.stream()
                        .filter(userToken -> !userToken.isMobile())
                        .findFirst()
                        .orElse(userTokens.get(0));
            } else {
                //tất cả các token đều là thiết bị di động,
                //chúng ta sẽ xóa token đầu tiên trong danh sách
                tokenToDelete = userTokens.get(0);
            }
            tokenRepository.delete(tokenToDelete);
        }
        long expirationInSeconds = expiration;
        Instant expirationDateTime = Instant.now().plusSeconds(expirationInSeconds);
        // Tạo mới một token cho người dùng
        Token newToken = Token.builder()
                .nguoiDung(user)
                .token(token)
                .revoked(false)
                .expired(false)
                .tokenType("Bearer")
                .expirationDate(expirationDateTime)
                .isMobile(isMobileDevice)
                .build();

        newToken.setRefreshToken(UUID.randomUUID().toString());
        newToken.setRefreshExpirationDate(Instant.now().plusSeconds(expirationRefreshToken));
        tokenRepository.save(newToken);
        return newToken;
    }

    /**
     * Lấy user từ refresh token, check revoked/expired trong DB
     */
    public NguoiDung getUserFromRefreshToken(String refreshToken) throws Exception {
        Token saved = tokenRepository.findByRefreshToken(refreshToken);
        if (Boolean.TRUE.equals(saved.isRevoked()) || Boolean.TRUE.equals(saved.isExpired())) {
            throw new IllegalStateException("Refresh token revoked or expired");
        }
        if (saved.getRefreshExpirationDate() != null &&
                saved.getRefreshExpirationDate()
                        .atZone(ZoneId.systemDefault())
                        .toInstant()
                        .isBefore(Instant.now())) {
            throw new IllegalStateException("Refresh token expired");
        }

        return saved.getNguoiDung();
    }

    /**
     * Revoke toàn bộ token còn hiệu lực của user (logout all / đổi mật khẩu)
     */
    @Transactional
    public void revokeAllTokensForUser(NguoiDung user) {
        tokenRepository.findAllValidTokensByUser(user.getId()).forEach(t -> {
            t.setRevoked(true);
            t.setExpired(true);
            tokenRepository.save(t);
        });
    }

    @Override
    @Transactional
    public void revokeToken(String token) throws Exception {
        Token existing = tokenRepository.findByToken(token);
        if (existing == null) {
            throw new DataNotFoundException("Token not found or already revoked");
        }
        existing.setRevoked(true);
        tokenRepository.save(existing);
    }
}
