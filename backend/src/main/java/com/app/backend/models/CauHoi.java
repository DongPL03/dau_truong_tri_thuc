package com.app.backend.models;


import com.app.backend.models.constant.LoaiNoiDung;
import com.fasterxml.jackson.annotation.JsonBackReference;
import com.fasterxml.jackson.annotation.JsonFormat;
import jakarta.persistence.*;
import lombok.*;

import java.time.Instant;

@Entity
@Table(name = "cau_hoi")
@Data
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class CauHoi {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JsonBackReference
    @JoinColumn(name = "bo_cau_hoi_id", nullable = false)
    private BoCauHoi boCauHoi;

    @Column(name = "do_kho")
    private String doKho;

    @Column(name = "noi_dung", nullable = false)
    private String noiDung;

    @Column(name = "loai_noi_dung")
    private String loaiNoiDung = LoaiNoiDung.VAN_BAN;

    @Column(name = "duong_dan_tep")
    private String duongDanTep;

    @Column(name = "lua_chon_a", nullable = false)
    private String luaChonA;

    @Column(name = "lua_chon_b", nullable = false)
    private String luaChonB;

    @Column(name = "lua_chon_c", nullable = false)
    private String luaChonC;

    @Column(name = "lua_chon_d", nullable = false)
    private String luaChonD;

    @Column(name = "dap_an_dung", length = 1, nullable = false)
    private Character dapAnDung; // A, B, C, D

    @Column(name = "giai_thich")
    private String giaiThich;

    @Column(name = "cap_nhat_luc")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant capNhatLuc;

    @PreUpdate
    protected void onUpdate() {
        capNhatLuc = Instant.now();
    }
}