package com.app.backend.models;

import com.fasterxml.jackson.annotation.JsonBackReference;
import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;

@Entity
@Table(name = "the_ghi_nho")
@Data
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class TheGhiNho {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JsonBackReference
    @JoinColumn(name = "phien_id", nullable = false)
    private PhienLuyenTap phien;

    @ManyToOne(fetch = FetchType.LAZY)
    @JsonBackReference
    @JoinColumn(name = "cau_hoi_id", nullable = false)
    private CauHoi cauHoi;

    @Column(name = "tao_luc", updatable = false)
    private LocalDateTime taoLuc;

    @PrePersist
    protected void onCreate() {
        taoLuc = LocalDateTime.now();
    }
}