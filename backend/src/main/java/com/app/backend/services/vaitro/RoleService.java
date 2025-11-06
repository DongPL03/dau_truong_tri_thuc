package com.app.backend.services.vaitro;

import com.app.backend.models.VaiTro;
import com.app.backend.repositories.IVaiTroRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@RequiredArgsConstructor
public class RoleService implements IRoleService {
    private final IVaiTroRepository roleRepository;

    @Override
    public List<VaiTro> getAllRoles() {
        return roleRepository.findAll();
    }
}
