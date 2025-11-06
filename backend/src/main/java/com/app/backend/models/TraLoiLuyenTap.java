package com.app.backend.models;

import com.fasterxml.jackson.annotation.JsonBackReference;
import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;

@Entity
@Table(name = "tra_loi_luyen_tap")
@Data
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class TraLoiLuyenTap {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JsonBackReference
    @JoinColumn(name = "phien_id", nullable = false)
    private PhienLuyenTap phienLuyenTap;

    @ManyToOne(fetch = FetchType.LAZY)
    @JsonBackReference
    @JoinColumn(name = "cau_hoi_id", nullable = false)
    private CauHoi cauHoi;

    @Column(name = "lua_chon", length = 1, nullable = false)
    private Character luaChon;

    @Column(name = "dung_hay_sai", nullable = false)
    private Boolean dungHaySai;

    @Column(name = "thoi_gian_ms")
    private Integer thoiGianMs;

    @Column(name = "tra_loi_luc", updatable = false)
    private LocalDateTime traLoiLuc;

    @PrePersist
    protected void onCreate() {
        traLoiLuc = LocalDateTime.now();
    }
}