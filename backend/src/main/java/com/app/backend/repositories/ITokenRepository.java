package com.app.backend.repositories;

import com.app.backend.models.NguoiDung;
import com.app.backend.models.Token;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;

public interface ITokenRepository extends JpaRepository<Token, Long> {
    List<Token> findByNguoiDung(NguoiDung nguoiDung);

    Token findByToken(String token);

    Token findByRefreshToken(String token);

    @Query("""
             select t from Token t 
             where t.nguoiDung.id = :userId and t.revoked = false and t.expired = false
            """)
    List<Token> findAllValidTokensByUser(@Param("userId") Long userId);

}
