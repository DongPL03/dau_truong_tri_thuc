package com.app.backend.models;

import com.fasterxml.jackson.annotation.JsonProperty;
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
    @JsonProperty("mo_ta")
    private String moTa;
}