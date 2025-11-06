package com.app.backend.models;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import lombok.*;

@Entity
@Table(name = "vai_tro")
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class VaiTro {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "ten_vai_tro", nullable = false)
    @JsonProperty("ten_vai_tro")
    private String tenVaiTro;

    public static String ADMIN = "ADMIN";
    public static String USER = "USER";
}
