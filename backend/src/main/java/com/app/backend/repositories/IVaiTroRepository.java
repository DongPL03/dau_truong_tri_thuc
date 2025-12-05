package com.app.backend.repositories;

import com.app.backend.models.VaiTro;
import org.springframework.data.jpa.repository.JpaRepository;

public interface IVaiTroRepository extends JpaRepository<VaiTro, Long> {
    VaiTro findByTenVaiTro(String tenVaiTro);
}
