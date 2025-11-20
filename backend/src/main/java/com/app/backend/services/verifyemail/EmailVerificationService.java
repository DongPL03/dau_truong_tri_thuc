package com.app.backend.services.verifyemail;

import com.app.backend.models.EmailVerificationToken;
import com.app.backend.models.NguoiDung;
import com.app.backend.repositories.IEmailVerifiTokenRepository;
import com.app.backend.repositories.INguoiDungRepository;
import com.app.backend.services.token.ITokenService;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Duration;
import java.time.Instant;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class EmailVerificationService implements IEmailVerificationService {

    private final IEmailVerifiTokenRepository tokenRepo;
    private final INguoiDungRepository userRepo;
    private final IEmailService emailService;
    private final ITokenService tokenService;

    @Value("${app.base-url:http://localhost:8080}")
    private String appBaseUrl;

    @Value("${app.email-verification.ttl-hours:24}")
    private long ttlHours;

    @Transactional
    @Override
    public void createAndSendTokenForUser(Long userId) throws Exception {
        NguoiDung user = userRepo.findById(userId)
                .orElseThrow(() -> new IllegalArgumentException("User not found"));

        // Xoá token cũ (nếu có) để gọn
        tokenRepo.deleteByNguoiDung_Id(userId);

        String token = UUID.randomUUID().toString();
        EmailVerificationToken evt = EmailVerificationToken.builder()
                .token(token)
                .nguoiDung(user)
                .createdAt(Instant.now())
                .expiresAt(Instant.now().plus(Duration.ofHours(ttlHours)))
                .build();
        tokenRepo.save(evt);

        String link = appBaseUrl + "/api/v1/users/verify-email?token=" + token; // nhớ khớp api.prefix
        String html = """
                    <h3>Verify your email</h3>
                    <p>Click the button below to activate your account:</p>
                    <p><a href="%s" style="background:#2563eb;color:#fff;padding:10px 16px;border-radius:8px;text-decoration:none;">Verify Email</a></p>
                    <p>Or open this link: %s</p>
                    <p>This link will expire in %d hours.</p>
                """.formatted(link, link, ttlHours);

        emailService.send("dongleepham@gmail.com", user.getEmail(), "Verify your email", html);
    }

    @Transactional
    @Override
    public void verifyToken(String token) throws Exception {
        EmailVerificationToken evt = tokenRepo.findByToken(token)
                .orElseThrow(() -> new IllegalArgumentException("Invalid verification token"));

        if (evt.isUsed()) throw new IllegalStateException("Verification token already used");
        if (evt.isExpired()) throw new IllegalStateException("Verification token expired");

        NguoiDung user = evt.getNguoiDung();
        user.setActive(true);            // kích hoạt tài khoản
        userRepo.save(user);

        // Đánh dấu token đã dùng
        evt.setUsedAt(Instant.now());
        tokenRepo.save(evt);

        // Revoke mọi token cũ nếu có (đảm bảo đăng nhập lại)
        tokenService.revokeAllTokensForUser(user);
    }

    @Transactional
    @Override
    public void resendForEmail(String email) throws Exception {
        NguoiDung user = userRepo.findByEmail(email)
                .orElseThrow(() -> new IllegalArgumentException("Email not found"));
        if (user.isActive()) {
            throw new IllegalStateException("Account already activated");
        }
        createAndSendTokenForUser(user.getId());
    }
}
