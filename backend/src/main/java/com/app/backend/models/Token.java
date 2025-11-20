package com.app.backend.models;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.fasterxml.jackson.annotation.JsonFormat;
import jakarta.persistence.*;
import lombok.*;

import java.time.Instant;

@Entity
@Table(name = "tokens")
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class Token {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "token", length = 255)
    private String token;

    @Column(name = "refresh_token", length = 255)
    private String refreshToken;

    @Column(name = "loai_token", length = 50)
    private String tokenType;

    @Column(name = "ngay_het_han")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant expirationDate;

    @Column(name = "refresh_ngay_het_han")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant refreshExpirationDate;

    @Column(name = "is_mobile", columnDefinition = "TINYINT(1)")
    private boolean isMobile;

    @Column(name = "thu_hoi", columnDefinition = "TINYINT(1)")
    private boolean revoked;

    @Column(name = "het_han", columnDefinition = "TINYINT(1)")
    private boolean expired;


    @ManyToOne
    @JsonBackReference
    @JoinColumn(name = "nguoi_dung_id")
    private NguoiDung nguoiDung;

}
