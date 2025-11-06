package com.app.backend.repositories;

import com.app.backend.models.ThongBao;
import org.springframework.data.jpa.repository.JpaRepository;

public interface IThongBaoRepository extends JpaRepository<ThongBao, Long> {
}
