package com.app.backend.models;

import com.fasterxml.jackson.annotation.JsonBackReference;
import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;

@Entity
@Table(name = "tin_nhan")
@Data
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class TinNhan {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JsonBackReference
    @JoinColumn(name = "tran_dau_id")
    private TranDau tranDau;

    @ManyToOne(fetch = FetchType.LAZY)
    @JsonBackReference
    @JoinColumn(name = "gui_boi_id", nullable = false)
    private NguoiDung guiBoi;

    @ManyToOne(fetch = FetchType.LAZY)
    @JsonBackReference
    @JoinColumn(name = "nhan_boi_id")
    private NguoiDung nhanBoi;

    @Column(name = "noi_dung", nullable = false)
    private String noiDung;

    @Column(name = "gui_luc", updatable = false)
    private LocalDateTime guiLuc;

    @PrePersist
    protected void onCreate() {
        guiLuc = LocalDateTime.now();
    }
}