package com.app.backend.repositories;

import com.app.backend.models.EmailVerificationToken;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface IEmailVerifiTokenRepository extends JpaRepository<EmailVerificationToken, Long> {
    Optional<EmailVerificationToken> findByToken(String token);
    void deleteByNguoiDung_Id(Long userId);
}
