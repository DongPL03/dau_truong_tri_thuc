package com.app.backend.components;

import com.app.backend.exceptions.InvalidParamException;
import com.app.backend.models.Token;
import io.jsonwebtoken.*;
import com.app.backend.models.NguoiDung;
import com.app.backend.repositories.ITokenRepository;
import io.jsonwebtoken.io.Decoders;
import io.jsonwebtoken.io.Encoders;
import io.jsonwebtoken.security.Keys;
import lombok.RequiredArgsConstructor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.stereotype.Component;

import javax.crypto.SecretKey;
import java.security.SecureRandom;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

@Component
@RequiredArgsConstructor
public class JwtTokenUtils {
    @Value("${jwt.expiration}")
    private int expiration; //save to an environment variable

    @Value("${jwt.expiration-refresh-token}")
    private int expirationRefreshToken;

    @Value("${jwt.secretKey}")
    private String secretKey;
    private static final Logger logger = LoggerFactory.getLogger(JwtTokenUtils.class);
    private final ITokenRepository tokenRepository;

    public String generateToken(NguoiDung nguoiDung) throws Exception {
        //properties => claims
        Map<String, Object> claims = new HashMap<>();
        // Add subject identifier (phone number or email)
        String subject = getSubject(nguoiDung);
        claims.put("subject", subject);
        // Add user ID
        claims.put("userId", nguoiDung.getId());
        claims.put("roles", nguoiDung.getAuthorities().stream()
                .map(GrantedAuthority::getAuthority)
                .toList());
        try {
            String token = Jwts.builder()
                    .claims(claims) //how to extract claims from this ?
                    .subject(subject)
                    .expiration(new Date(System.currentTimeMillis() + expiration * 1000L))
                    .signWith(getSignInKey(), Jwts.SIG.HS256)
                    .compact();
            return token;
        } catch (Exception e) {
            //you can "inject" Logger, instead System.out.println
            throw new InvalidParamException("Cannot create jwt token, error: " + e.getMessage());
            //return null;
        }
    }

    private static String getSubject(NguoiDung nguoiDung) {
        // Determine subject identifier (phone number or email)
        String subject = nguoiDung.getTenDangNhap();
        if (subject == null || subject.isBlank()) {
            // If phone number is null or blank, use email as subject
            subject = nguoiDung.getEmail();
        }
        return subject;
    }

    public List<String> extractRoles(String token) {
        Claims claims = extractAllClaims(token);
        Object rolesObj = claims.get("roles");
        if (rolesObj instanceof List<?>) {
            return ((List<?>) rolesObj).stream()
                    .filter(String.class::isInstance)
                    .map(String.class::cast)
                    .toList();
        }
        return List.of();
    }

    private SecretKey getSignInKey() {
        byte[] bytes = Decoders.BASE64.decode(secretKey);
        //Keys.hmacShaKeyFor(Decoders.BASE64.decode("TaqlmGv1iEDMRiFp/pHuID1+T84IABfuA0xXh4GhiUI="));
        return Keys.hmacShaKeyFor(bytes);
    }


    private String generateSecretKey() {
        SecureRandom random = new SecureRandom();
        byte[] keyBytes = new byte[32]; // 256-bit key
        random.nextBytes(keyBytes);
        String secretKey = Encoders.BASE64.encode(keyBytes);
        return secretKey;
    }

    private Claims extractAllClaims(String token) {
        return Jwts.parser()  // Khởi tạo JwtParserBuilder
                .verifyWith(getSignInKey())  // Sử dụng verifyWith() để thiết lập signing key
                .build()  // Xây dựng JwtParser
                .parseSignedClaims(token)  // Phân tích token đã ký
                .getPayload();  // Lấy phần body của JWT, chứa claims
    }


    public <T> T extractClaim(String token, Function<Claims, T> claimsResolver) {
        final Claims claims = this.extractAllClaims(token);
        return claimsResolver.apply(claims);
    }

    //check expiration
    public boolean isTokenExpired(String token) {
        Date expirationDate = this.extractClaim(token, Claims::getExpiration);
        return expirationDate.before(new Date());
    }

    public String getSubject(String token) {
        return extractClaim(token, Claims::getSubject);
    }

    public boolean validateToken(String token, NguoiDung userDetails) {
        try {
            String subject = extractClaim(token, Claims::getSubject);
            System.out.println("🎯 Subject in token: " + subject);
            System.out.println("🧍 UserDetails username: " + userDetails.getUsername());
            Token existingToken = tokenRepository.findByToken(token);
            if (existingToken == null || existingToken.isRevoked() || !userDetails.isActive()) {
                System.out.println("🚫 Token bị revoke hoặc user không active");
                return false;
            }
            boolean result = (subject.equals(userDetails.getUsername())) && !isTokenExpired(token);
            System.out.println("✅ Token hợp lệ? " + result);
            return result;
        } catch (Exception e) {
            e.printStackTrace();
            return false;
        }
    }
}
