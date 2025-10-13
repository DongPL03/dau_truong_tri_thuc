package com.app.backend.models;

import jakarta.persistence.*;
import lombok.*;

@Entity
@Table(name = "chu_de")
@Data
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ChuDe {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "ten", length = 100, nullable = false, unique = true)
    private String ten;

    @Column(name = "mo_ta")
    private String moTa;
}