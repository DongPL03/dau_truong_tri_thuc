package com.app.backend.models;

import com.fasterxml.jackson.annotation.JsonFormat;
import jakarta.persistence.Column;
import jakarta.persistence.MappedSuperclass;
import jakarta.persistence.PrePersist;
import jakarta.persistence.PreUpdate;
import lombok.Getter;
import lombok.Setter;

import java.time.LocalDateTime;

@Getter
@Setter
@MappedSuperclass
public class BaseEntity {
    @Column(name = "tao_luc", updatable = false)
    @JsonFormat(pattern = "yyyy-MM-dd'T'HH:mm:ss", shape = JsonFormat.Shape.STRING)
    private LocalDateTime taoLuc;

    @Column(name = "cap_nhat_luc")
    @JsonFormat(pattern = "yyyy-MM-dd'T'HH:mm:ss", shape = JsonFormat.Shape.STRING)
    private LocalDateTime capNhatLuc;

    @PrePersist
    protected void onCreate() {
        taoLuc = LocalDateTime.now();
        capNhatLuc = LocalDateTime.now();
    }

    @PreUpdate
    protected void onUpdate() {
        capNhatLuc = LocalDateTime.now();
    }
}
